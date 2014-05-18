package com.asci.env.primitives

import com.asci.env.Env
import com.asci.{InvalidArgsNumber, OtherError, EvalError, Expr}
import com.asci.Eval.Eval
import com.asci.Expr.Atom
import com.asci.Constant.BooleanConstant

object Internal {
  def define(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
    case Atom(a) :: b :: Nil => Right((e.insert(a, b), b))
    case a :: _ :: Nil => Left(OtherError("Cannot assign to non-identifier"))
    case _ => Left(InvalidArgsNumber(2))
  }

  implicit class Trueness(val e: Expr) {
    def isTrue: Boolean = e match {
      case BooleanConstant(f) => f
    }
  }

  def `if`(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
    case cond :: t :: f :: Nil =>
      val condition = cond.eval(e)

      condition match {
        case r@Right((_, expr)) =>
          (if (expr.isTrue) t else f).eval(e)
        case l@Left(_) => l
      }
    case _ => Left(InvalidArgsNumber(3))
  }

  def fst[A](a: (A, A)): A = a._1
  def snd[A](a: (A, A)): A = a._2
}
