package com.asci

import com.asci.env.Env
import com.asci.Expr._
import scala.Tuple1
import scala.Some
import com.asci.Expr._
import com.asci.Constant._
import scalaz._
import Scalaz._

object Eval {
  implicit class Eval(val expr: Expr) {
    def eval(env: Env): \/[EvalError, (Env, Expr)] = expr match {
      case ListExpr(f :: args) =>
        for {
          evaluatedFun <- f.eval(env)
          applied <- apply(evaluatedFun._1, evaluatedFun._2, args)
        } yield applied
      case v@IntegerNum(_) => (env, v).right
      case v@FloatingNum(_) => (env, v).right
      case Quotation(q) => (env, q).right
      case s@StringConstant(_) => (env, s).right
      case b@BooleanConstant(_) => (env, b).right
      case Atom(a) => env.get(a) match {
        case Some(exp) => (env, exp).right
        case None      => UnboundVariable(a).left
      }
      case c@CharacterConstant(_) => (env, c).right
      case _ => NotImplemented("foo").left
    }

    private def apply[A, B](env: Env, f: Expr, args: List[Expr]): \/[EvalError, (Env, Expr)] = {
      f match {
        case x: ExprFun => x.f(env, args)
        // FIXME: more type-safety
        // FIXME: allow variable arity implementation for non-associative operations
        case x: FunWrap[A, B] =>
          for {
            evaluatedArgs <- args.map(_.eval(env)).sequenceU
            args1 = evaluatedArgs.map(_._2)
            result <- x.arity match {
              case Fixed(i) =>
                args1.length match {
                  case j if i == j =>
                    try {
                      val tupledArgs = args1.tupleize(i).asInstanceOf[B]
                      (env, Expr(x.f(tupledArgs))).right
                    } catch {
                      case e: EvalError => e.left
                    }
                  case j =>
                    InvalidArgsNumber(i).left
                }
              case Variable =>
                try {
                  val res = args1.tail.foldLeft(args1.head) {case (acc, v) => Expr(x.f((acc, v).asInstanceOf[B]))}
                  (env, res).right
                } catch {
                  // FIXME: be more specific about exception type
                  case e: Exception => TypeMismatch("FIXME unknown type", "FIXME unknown type").left
                }
            }
          } yield result
        case Lambda(formals, body, closure) => formals match {
          case ListExpr(vars) if vars.length == args.length =>

            val result: \/[EvalError, Expr] = for {
              evaluatedArgs <- args.map(_.eval(env)).sequenceU
              args1 = evaluatedArgs.map(_._2)
              envWithVars = vars.zip(args1).foldLeft(env) {
                case (acc, (Atom(name), value)) => acc.insert(name, value)
              }
              finalEnv = envWithVars |+| closure
              result <- body.eval(finalEnv)
            } yield result._2

            result.map(expr => (env, expr))
          case ListExpr(vars) => InvalidArgsNumber(vars.length).left
          case Atom(variable) =>
            val result: \/[EvalError, Expr] = for {
              evaluatedArgs <- args.map(_.eval(env)).sequenceU
              args1 = evaluatedArgs.map(_._2)
              envWithArg = env.insert(variable, ListExpr(args1))
              finalEnv = envWithArg |+| closure
              result <- body.eval(finalEnv)
            } yield result._2

            result.map(expr => (env, expr))
          case DottedList(vars, Atom(leftovers)) if args.length >= vars.length =>
            val result: \/[EvalError, Expr] = for {
              evaluatedArgs <- args.map(_.eval(env)).sequenceU
              args1 = evaluatedArgs.map(_._2)
              envWithVars = vars.zip(args1).foldLeft(env) {
                case (acc, (Atom(name), value)) => env.insert(name, value)
              }
              envWithLeftovers = envWithVars.insert(leftovers, ListExpr(args1.drop(vars.length)))
              finalEnv = envWithLeftovers |+| closure
              result <- body.eval(finalEnv)
            } yield result._2

            result.map(expr => (env, expr))
          case DottedList(vars, _) =>
            InvalidArgsNumber(vars.length).left
        }
        case y => OtherError(s"$f is not a function").left
      }
    }
  }

  private implicit class ListTuple[A](val l: List[A]) {
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
}
