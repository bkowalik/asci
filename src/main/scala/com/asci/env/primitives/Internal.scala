package com.asci.env.primitives

import com.asci.env.Env
import com.asci._
import com.asci.Eval.Eval
import com.asci.Expr.{Lambda, DottedList, Atom, ListExpr}
import com.asci.Constant.BooleanConstant
import com.asci.ImplicitUtils.Sequencable
import com.asci.InvalidArgsNumber
import com.asci.OtherError

object Internal {
  def define(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
    case Atom(a) :: b :: Nil =>
      val evaled = b.eval(e)
      evaled match {
        case Right((_, evaled1)) => Right((e.insert(a, evaled1), b))
        case Left(err) => Left(err)
      }
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

  def let(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
    case ListExpr(bindings) :: body :: Nil =>
      val binds = bindings.map({
        case ListExpr(Atom(atom) :: value :: Nil) => value.eval(e) match {
          case Right((_, v)) => Right((atom, v))
          case Left(l)       => Left(l)
        }
      }).sequence

      def distinct(l: List[(String, Expr)]) = {
        val names = l map (_._1)
        names.distinct == names
      }

      binds match {
        case Right(environmentOverlay) if distinct(environmentOverlay) =>
          val newEnv = environmentOverlay.foldLeft(e) {case (acc, (s, v)) => e.insert(s, v)}
          body.eval(newEnv) match {
            case Right((_, result)) => Right(e, result)
            case Left(l) => Left(l)
          }
        case Right(_) => Left(OtherError("Duplicate bound variable"))
        case Left(l) => Left(l)
      }
    case bindings :: _ :: Nil => Left(TypeMismatch("list of lists", bindings.getClass.toString))
    case _ => Left(InvalidArgsNumber(2))
  }

  def lambda(e: Env, args: List[Expr]): Either[EvalError, (Env, Expr)] = args match {
    case (f@ListExpr(formals)) :: body :: Nil =>
      val arguments = formals.map({
        case Atom(x) => Right(x)
        case a       => Left(TypeMismatch("atom", a.toString))
      }).sequence

      arguments match {
        case Right(_) => Right((e, Lambda(f, body, e)))
        case Left(l)  => Left(l)
      }
    case DottedList(_, _) :: _ :: Nil => Left(NotImplemented("n or more arguments lambda"))
    case Atom(_) :: _ :: Nil => Left(NotImplemented("variable arity lambda"))
    case _ => Left(InvalidArgsNumber(2))
  }

  def fst[A](a: (A, A)): A = a._1
  def snd[A](a: (A, A)): A = a._2
}
