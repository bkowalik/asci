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

object Eval {
  implicit class Eval(val expr: Expr) {
    def eval(env: Env): Either[EvalError, (Env, Expr)] = expr match {
      case ListExpr(f :: args) =>
        val evaluatedFun = f.eval(env)
        evaluatedFun match {
          case Right((newEnv, fun)) =>
            apply(newEnv, fun, args)
          case l@Left(_) => l
        }
      case v@IntegerNum(_) => Right((env, v))
      case v@FloatingNum(_) => Right((env, v))
      case Quotation(q) => Right((env, q))
      case s@StringConstant(_) => Right((env, s))
      case b@BooleanConstant(_) => Right((env, b))
      case Atom(a) => env.get(a) match {
        case Some(exp) => Right((env, exp))
        case None      => Left(UnboundVariable(a))
      }
      case _ => Left(NotImplemented("foo"))
    }

    private def apply[A, B](env: Env, f: Expr, args: List[Expr]): Either[EvalError, (Env, Expr)] = {
      f match {
        case x: ExprFun => x.f(env, args)
        // FIXME: more type-safety
        // FIXME: allow variable arity implementation for non-associative operations
        case x: FunWrap[A, B] =>
          val evaluatedArgs = args.map(_.eval(env)).sequence

          evaluatedArgs match {
            case Right(l) =>
              val args1 = l map (_._2)

              x.arity match {
                case Fixed(i) =>
                  args1.length match {
                    case j if i == j =>
                      try {
                        val tupledArgs = args1.tupleize(i).asInstanceOf[B]
                        Right((env, Expr(x.f(tupledArgs))))
                      } catch {
                        case e: EvalError => Left(e)
                      }
                    case _ => Left(InvalidArgsNumber(i))
                  }
                case Variable =>
                  try {
                    val res = args1.tail.foldLeft(args1.head) {case (acc, v) => Expr(x.f((acc, v).asInstanceOf[B]))}
                    Right((env, res))
                  } catch {
                    // FIXME: be more specific about exception type
                    case e: Exception => Left(TypeMismatch("FIXME unknown type", "FIXME unknown type"))
                  }
              }

            case Left(l) => Left(l)
          }
        case Lambda(formals, body, closure) => formals match {
          case ListExpr(vars) if vars.length == args.length =>
            val evaluatedArgs = args.map(_.eval(env)).sequence

            evaluatedArgs match {
              case Right(l) =>
                val args1 = l map (_._2)

                val envWithArgs = vars.zip(args1).foldLeft(env) {case (acc, (Atom(name), value)) => env.insert(name, value)}
                val finalEnv = EnvMonoid.append(envWithArgs, closure)
                val result = body.eval(finalEnv)

                result match {
                  case Right((_, r)) => Right(env, r)
                  case Left(l) => Left(l)
                }
              case Left(err) => Left(err)
            }
          case ListExpr(vars) => Left(InvalidArgsNumber(vars.length))
        }
        case y => Left(OtherError(s"$f is not a function"))
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
