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

  it should "implement if" in new EnvSupplier {
    val result = eval(env, "((if #f + *) 3 4)")
    result shouldBe a [Right[_,_]]
    result.right.get._2 should equal(IntegerNum(12))

    val result2 = eval(env, "(if #t + *)")
    result2 shouldBe a [Right[_,_]]
    result2.right.get._2 shouldBe a [FunWrap[_,_]]

    val result3 = eval(env, "(if #f + (if #f - *))")
    result3 shouldBe a [Right[_,_]]
    result3.right.get._2 shouldBe a [FunWrap[_,_]]

    val result4 = eval(env, "(if #t '+ '*)")
    result4 shouldBe a [Right[_,_]]
    result4.right.get._2 should equal(Atom("+"))
  }

  it should "implement let" in new EnvSupplier {
    val result = eval(env, "(let ((x 1)) x)")
    result shouldBe a [Right[_,_]]
    result.right.get._1.get("x") should equal(None)
    result.right.get._2 should equal(IntegerNum(1))
  }

  it should "fail on duplicate bound variable in let" in new EnvSupplier {
    val result = eval(env, "(let ((x 1) (x 2)) x)")
    result should not be an [Right[_,_]]
  }

  def eval(env: Env, scheme: String): Either[EvalError, (Env, Expr)] = {
    import com.asci.Parser
    import com.asci.Eval._

    val parser = new Parser
    parser.read(scheme) match {
      case parser.Success(result: List[Expr], _) => result.head.eval(env)
      case parser.Error(err, _) => Left(ParseError(err))
      case parser.Failure(err, _) => Left(ParseError(err))
    }
  }
}
