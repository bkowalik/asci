package com.asci

import com.asci.env.Env
import com.asci.Expr._
import com.asci.Expr.Atom
import scala.Tuple1
import scala.Some
import com.asci.Expr.Lambda
import com.asci.Constant.FloatingNum
import com.asci.Expr.ExprFun
import com.asci.Expr.FunWrap
import com.asci.Expr.Quotation
import com.asci.Expr.ListExpr
import com.asci.Constant.BooleanConstant
import com.asci.Constant.IntegerNum
import com.asci.Constant.StringConstant
import com.asci.Expr.Fixed
import com.asci.env.Env.EnvMonoid
import com.asci.ImplicitUtils.Sequencable
import scalaz._
import Scalaz._
import scalaz.\/._

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
      case _ => NotImplemented("foo").left
    }

    private def apply[A, B](env: Env, f: Expr, args: List[Expr]): \/[EvalError, (Env, Expr)] = {
      f match {
        case x: ExprFun => fromEither(x.f(env, args))
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
              envWithArgs = vars.zip(args1).foldLeft(env) {
                case (acc, (Atom(name), value)) => env.insert(name, value)
              }
              finalEnv = EnvMonoid.append(envWithArgs, closure)
              result <- body.eval(finalEnv)
            } yield result._2

            result.map(expr => (env, expr))
          case ListExpr(vars) => InvalidArgsNumber(vars.length).left
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
