package com.asci

import org.scalatest.{FlatSpec, Matchers}
import com.asci.Expr.{ExprFun, ListExpr, Atom}
import com.asci.Constant.IntegerNum

class EvalTest extends FlatSpec with Matchers {
  behavior of "eval"

  trait EnvSupplier {
    def define(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
      case Atom(a) :: b :: Nil => Right((e.insert(a, b), b))
      case a :: _ :: Nil => Left(OtherError("Cannot assign to non-identifier"))
      case _ => Left(InvalidArgsNumber(2))
    }

    val env = new Env().insert("define", ExprFun(define))
  }

  it should "define variable" in new EnvSupplier {
    val result = eval(env, "(define x 20)")
    result shouldBe a [Right[_,_]]
    result.right.get._1.get("x").get should equal(IntegerNum(20))
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
