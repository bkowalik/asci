package com.asci.env.primitives

import com.asci.env.Env
import com.asci.{InvalidArgsNumber, OtherError, EvalError, Expr}
import com.asci.Expr.Atom

object Internal {
  def define(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
    case Atom(a) :: b :: Nil => Right((e.insert(a, b), b))
    case a :: _ :: Nil => Left(OtherError("Cannot assign to non-identifier"))
    case _ => Left(InvalidArgsNumber(2))
  }

  def fst[A](a: (A, A)): A = a._1
  def snd[A](a: (A, A)): A = a._2
}
