package com.asci

import org.scalatest.{FlatSpec, Matchers}
import com.asci.Expr._
import com.asci.Constant.Num
import com.asci.Expr.Atom
import scala.Some
import com.asci.Constant.FloatingNum
import com.asci.Expr.ExprFun
import com.asci.Expr.Quotation
import com.asci.Expr.ListExpr
import com.asci.Constant.IntegerNum

class EvalTest extends FlatSpec with Matchers {
  behavior of "eval"

  trait EnvSupplier {
    def define(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
      case Atom(a) :: b :: Nil => Right((e.insert(a, b), b))
      case a :: _ :: Nil => Left(OtherError("Cannot assign to non-identifier"))
      case _ => Left(InvalidArgsNumber(2))
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
                                 "car" -> ExprFun(car))

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
    case _ => Left(NotImplemented("foo"))
  }

  def apply(env: Env, f: String, args: List[Expr]): Either[EvalError, (Env, Expr)] = {
    env.get(f) match {
      case Some(x: ExprFun) => x.f(env, args)
      case Some(y) => Left(OtherError(s"$f is not a function"))
      case None => Left(UnboundVariable(f))
    }
  }
}
