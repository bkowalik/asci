package com.asci

import org.scalatest.{FlatSpec, Matchers}
import com.asci.Expr._
import com.asci.Constant.{StringConstant, Num, FloatingNum, IntegerNum}
import com.asci.Expr.Atom
import scala.Some
import com.asci.Expr.ExprFun
import com.asci.Expr.Quotation
import com.asci.Expr.ListExpr

class EvalTest extends FlatSpec with Matchers {
  behavior of "eval"

  trait EnvSupplier {

    def define(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
      case Atom(a) :: b :: Nil => Right((e.insert(a, b), b))
      case a :: _ :: Nil => Left(OtherError("Cannot assign to non-identifier"))
      case _ => Left(InvalidArgsNumber(2))
    }

    def fst[A](a: (A, A)): A = a._1
    def snd[A](a: (A, A)): A = a._2

    def concat[A](a: (A, A)): A = a match {
      case (StringConstant(s1), StringConstant(s2)) => StringConstant(s1 + s2).asInstanceOf[A]
    }

    def add[T](e: Env, args: List[Expr])(implicit num: Numeric[T]): Either[EvalError, (Env, Expr)] = args match {
      case a :: as =>
        val evaluated = evalInternal(e, a)
        val sumOfRest = add(e, as)
        try {
          sumOfRest.right.get._2 match {
            case v: Num[T] => evaluated.right.get._2 match {
              case v2: Num[T] => Right((e, Num.+(v, v2)))
            }
          }
        } catch {
          // FIXME: specify exception type
          case e: Exception => Left(TypeMismatch("number", "FIXME unknown type"))
        }
      case Nil => Right((e, IntegerNum(0)))
      case _ => Left(OtherError("FIXME: unknown error in add"))
    }

    def car(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
      case a :: Nil =>
        val evaluated = evalInternal(e, a)
        try {
          evaluated.right.get._2 match {
            case ListExpr(head :: _) => Right((e, head))
            case DottedList(head :: _, _) => Right((e, head))
          }
        } catch {
          // FIXME
          case e: Exception => Left(TypeMismatch("list", "unknown"))
        }
      case _ => Left(OtherError("Unknown error in car"))
    }

    private val initialEnv = Map("define" -> ExprFun(define),
                                 "+" -> ExprFun(add[Float]),
                                 "car" -> ExprFun(car),
                                 "concat" -> FunWrap(concat[StringConstant], Variable()),
                                 "fst" -> FunWrap(fst[Expr], Fixed(2)),
                                 "snd" -> FunWrap(snd[Expr], Fixed(2)))

    val env = new Env(initialEnv)
  }

  it should "define variable" in new EnvSupplier {
    val result = eval(env, "(define x 20)")
    result shouldBe a [Right[_,_]]
    result.right.get._1.get("x").get should equal(IntegerNum(20))
  }

  it should "add 2 numbers" in new EnvSupplier {
    val result = eval(env, "(+ 2 2)")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(IntegerNum(4))
  }

  it should "add 2 floating numbers" in new EnvSupplier {
    val result = eval(env, "(+ 2.0 1.03)")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(FloatingNum(2.0f + 1.03f))
  }

  it should "add 2 mixed numbers" in new EnvSupplier {
    val result = eval(env, "(+ 2 4.5)")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(FloatingNum(2 + 4.5f))

    val result1 = eval(env, "(+ 4.6 1)")
    result1 shouldBe a [Right[_,_]]
    result1.right.get._2 should equal(FloatingNum(4.6f + 1))
  }

  it should "add n mixed numbers" in new EnvSupplier {
    val result = eval(env, "(+ 1 3.4 10 4.2 5.4 0.113 124.32)")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(FloatingNum(1 + 3.4f + 10 + 4.2f + 5.4f + 0.113f + 124.32f))
  }

  it should "fail on adding non-numbers" in new EnvSupplier {
    val result = eval(env, "(+ 1 3.2 foo)")
    result should not be an [Right[_,_]]
  }

  it should "eval nested expressions while adding" in new EnvSupplier {
    val result = eval(env, "(+ 1 2 (+ 3 4))")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(IntegerNum(1+2+3+4))
  }

  it should "car proper list" in new EnvSupplier {
    val result = eval(env, "(car '(1 2 3))")
    result.right.get._2 should equal(IntegerNum(1))
  }

  it should "car dotted list" in new EnvSupplier {
    val result = eval(env, "(car '(1 2 . 3))")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(IntegerNum(1))
  }

  it should "fail on car of empty list" in new EnvSupplier {
    val result = eval(env, "(car '())")
    result should not be an [Right[_,_]]
  }

  it should "concat strings" in new EnvSupplier {
    val result = eval(env, "(concat \"foo\" \"bar\")")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(StringConstant("foobar"))
  }

  it should "get first tuple element" in new EnvSupplier {
    val result = eval(env, "(fst \"foo\" \"bar\")")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(StringConstant("foo"))

    val result2 = eval(env, "(fst 1 2)")
    result2 shouldBe a [Right[_,_]]
    result2.right.get._2 should equal(IntegerNum(1))
  }

  it should "get second tuple element" in new EnvSupplier {
    val result = eval(env, "(snd \"foo\" \"bar\")")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(StringConstant("bar"))
  }

  def eval(env: Env, scheme: String): Either[EvalError, (Env, Expr)] = {
    import com.asci.Parser

    val parser = new Parser
    parser.read(scheme) match {
      case parser.Success(result: List[Expr], _) => evalInternal(env, result.head)
      case parser.Error(err, _) => Left(ParseError(err))
      case parser.Failure(err, _) => Left(ParseError(err))
    }
  }

  def evalInternal(env: Env, expr: Expr): Either[EvalError, (Env, Expr)] = expr match {
    case ListExpr(Atom(f) :: args) => apply(env, f, args)
    case v@IntegerNum(_) => Right((env, v))
    case v@FloatingNum(_) => Right((env, v))
    case Quotation(q) => Right((env, q))
    case s@StringConstant(_) => Right((env, s))
    case _ => Left(NotImplemented("foo"))
  }

  def tupleize1[A](l: List[A]): Tuple1[A] = l match {
    case a :: Nil => Tuple1(a)
  }

  def tupleize2[A](l: List[A]): Tuple2[A, A] = l match {
    case a :: b :: Nil => (a, b)
  }

  def tupleize[A](l: List[A], n: Int) = n match {
    case 1 => tupleize1(l)
    case 2 => tupleize2(l)
  }

  def apply[A, B](env: Env, f: String, args: List[Expr]): Either[EvalError, (Env, Expr)] = {
    env.get(f) match {
      case Some(x: ExprFun) => x.f(env, args)
      // FIXME: probably needs to evaluate arguments on the go
      // FIXME: more type-safety
      // FIXME: allow variable arity implementation for non-associative operations
      case Some(x: FunWrap[A, B]) =>
        val f: Function[B, A] = x.f
        x.arity match {
          case Fixed(i) =>
            args.length match {
              case j if i == j => Right((env, Expr(f(tupleize(args, i).asInstanceOf[B]))))
              case _           => Left(InvalidArgsNumber(i))
            }
          case Variable() =>
            val a = args.tail.foldLeft(args.head) {case (acc: B, v: Expr) => Expr(f((acc, v).asInstanceOf[B])) }
            Right((env, a))
        }
      case Some(y) => Left(OtherError(s"$f is not a function"))
      case None => Left(UnboundVariable(f))
    }
  }
}
