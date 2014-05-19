package com.asci.env.primitives

import com.asci.env.Env
import com.asci._
import com.asci.Eval.Eval
import com.asci.Expr.{Lambda, DottedList, Atom, ListExpr}
import com.asci.Constant.BooleanConstant
import com.asci.InvalidArgsNumber
import com.asci.OtherError
import scalaz._
import Scalaz._

object Internal {
  def define(e: Env, args: List[Expr]): \/[EvalError, (Env, Expr)] = args match {
    case Atom(a) :: b :: Nil =>
      for {
        body <- b.eval(e)
      } yield {
        val newEnv = e.insert(a, body._2)
        (newEnv, b)
      }
    case a :: _ :: Nil => OtherError("Cannot assign to non-identifier").left
    case _ => InvalidArgsNumber(2).left
  }

  implicit class Trueness(val e: Expr) {
    def isTrue: Boolean = e match {
      case BooleanConstant(f) => f
    }
  }

  def `if`(e: Env, args: List[Expr]): \/[EvalError, (Env, Expr)] = args match {
    case cond :: t :: f :: Nil =>
      for {
        condition <- cond.eval(e)
        result <- (if (condition._2.isTrue) t else f).eval(e)
      } yield result
    case _ => InvalidArgsNumber(3).left
  }

  def let(e: Env, args: List[Expr]): \/[EvalError, (Env, Expr)] = args match {
    case ListExpr(bindings) :: body :: Nil =>
      val result: \/[EvalError, Expr] = for {
        binds <- bindings.map({
          // FIXME: better error message for messed up binding than MatchError
          case ListExpr(Atom(atom) :: value :: Nil) => value.eval(e).map({case (_, v) => (atom, v)})
        }).sequenceU
        environmentOverlay <- binds.asDistinct
        newEnv <- environmentOverlay.foldLeft(e) {case (acc, (s, v)) => e.insert(s, v)}.right
        evaled <- body.eval(newEnv)
      } yield evaled._2

      result.map(expr => (e, expr))
    case bindings :: _ :: Nil => TypeMismatch("list of lists", bindings.getClass.toString).left
    case _ => InvalidArgsNumber(2).left
  }

  def lambda(e: Env, args: List[Expr]): \/[EvalError, (Env, Expr)] = args match {
    case (f@ListExpr(formals)) :: body :: Nil =>
      for {
        arguments <- formals.map({
          case Atom(x) => x.right
          case a       => TypeMismatch("atom", a.toString).left
        }).sequenceU
      } yield (e, Lambda(f, body, e))
    case DottedList(_, _) :: _ :: Nil => NotImplemented("n or more arguments lambda").left
    case (a@Atom(_)) :: body :: Nil =>
      (e, Lambda(a, body, e)).right
    case _ => InvalidArgsNumber(2).left
  }

  def fst[A](a: (A, A)): A = a._1
  def snd[A](a: (A, A)): A = a._2

  implicit class Distinct(l: List[(String, Expr)]) {
    def asDistinct: \/[EvalError, List[(String, Expr)]] = {
      val names = l map (_._1)
      if (names.distinct == names)
        l.right
      else
        OtherError("Duplicate bound variable").left
    }
  }
}
