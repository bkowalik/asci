package com.asci

import org.scalatest.{FlatSpec, Matchers}
import com.asci.Expr._
import com.asci.Constant.{StringConstant, Num, FloatingNum, IntegerNum}
import com.asci.Expr.Atom
import scala.Some
import com.asci.Expr.ExprFun
import com.asci.Expr.Quotation
import com.asci.Expr.ListExpr
import com.asci.env.Env

class EvalTest extends FlatSpec with Matchers {
  behavior of "eval"

  trait EnvSupplier {
    val env = Env.r5rsEnv
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

  it should "sub n mixed numbers" in new EnvSupplier {
    val result = eval(env, "(- 2.04 5.06 1 923.4451)")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(FloatingNum(2.04f - 5.06f - 1 - 923.4451f))
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

  it should "work with nested expression for fixed arity function" in new EnvSupplier {
    val result = eval(env, "(snd 1 (fst 2 3))")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(IntegerNum(2))
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

  implicit class ListTuple[A](val l: List[A]) {
    def tupleize(n: Int) = n match {
      case 1 => tupleize1(l)
      case 2 => tupleize2(l)
    }

    private def tupleize1(l: List[A]): Tuple1[A] = l match {
      case a :: Nil => Tuple1(a)
    }

    private def tupleize2(l: List[A]): (A, A) = l match {
      case a :: b :: Nil => (a, b)
    }
  }

  def apply[A, B](env: Env, f: String, args: List[Expr]): Either[EvalError, (Env, Expr)] = {
    env.get(f) match {
      case Some(x: ExprFun) => x.f(env, args)
      // FIXME: more type-safety
      // FIXME: allow variable arity implementation for non-associative operations
      case Some(x: FunWrap[A, B]) =>
        val evaluatedArgs = args.map(e => evalInternal(env, e)).partition(_.isLeft) match {
          // WTF Scala? no sequence? really?
          case (Nil,   as) => Right(for(Right(i) <- as) yield i)
          case (errors, _) => Left(errors.head)
        }

        evaluatedArgs match {
          case Right(l) =>
            val args1 = l map (_._2)

            x.arity match {
              case Fixed(i) =>
                args1.length match {
                  case j if i == j =>
                    try {
                      val tupledArgs = args1.tupleize(i).asInstanceOf[B]
                      Right((env, Expr(x.f(tupledArgs))))
                    } catch {
                      case e: EvalError => Left(e)
                    }
                  case _ => Left(InvalidArgsNumber(i))
                }
              case Variable =>
                try {
                  val res = args1.tail.foldLeft(args1.head) {case (acc, v) => Expr(x.f((acc, v).asInstanceOf[B]))}
                  Right((env, res))
                } catch {
                  // FIXME: be more specific about exception type
                  case e: Exception => Left(TypeMismatch("FIXME unknown type", "FIXME unknown type"))
                }
            }

          case Left(l) => l
        }

      case Some(y) => Left(OtherError(s"$f is not a function"))
      case None => Left(UnboundVariable(f))
    }
  }
}
