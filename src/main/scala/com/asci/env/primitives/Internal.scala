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
import scalaz.\/._

object Internal {
  def define(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
    case Atom(a) :: b :: Nil =>
      (for {
        body <- b.eval(e)
      } yield {
        val newEnv = e.insert(a, body._2)
        (newEnv, b)
      }).toEither
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
      (for {
        condition <- cond.eval(e)
        result <- (if (condition._2.isTrue) t else f).eval(e)
      } yield result).toEither
    case _ => Left(InvalidArgsNumber(3))
  }

  def let(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
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

      result.map(expr => (e, expr)).toEither
    case bindings :: _ :: Nil => Left(TypeMismatch("list of lists", bindings.getClass.toString))
    case _ => Left(InvalidArgsNumber(2))
  }

  def lambda(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
    case (f@ListExpr(formals)) :: body :: Nil =>
      import ImplicitUtils.Sequencable
      for {
        arguments <- formals.map({
          case Atom(x) => Right(x)
          case a       => Left(TypeMismatch("atom", a.toString))
        }).sequence.right
      } yield (e, Lambda(f, body, e))
    case DottedList(_, _) :: _ :: Nil => Left(NotImplemented("n or more arguments lambda"))
    case Atom(_) :: _ :: Nil => Left(NotImplemented("variable arity lambda"))
    case _ => Left(InvalidArgsNumber(2))
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
