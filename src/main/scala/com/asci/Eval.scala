package com.asci

import com.asci.env.Env
import com.asci.Expr._
import com.asci.Expr.Atom
import scala.Some
import com.asci.Constant.{BooleanConstant, FloatingNum, IntegerNum, StringConstant}
import com.asci.Expr.ExprFun
import com.asci.Expr.FunWrap
import com.asci.Expr.Quotation
import com.asci.Expr.ListExpr
import com.asci.Expr.Fixed

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
          val evaluatedArgs = args.map(_.eval(env)).partition(_.isLeft) match {
            // WTF Scala? no sequence? really?
            case (Nil,   as) => Right(for(Right(i) <- as) yield i)
            case (errors, _) => Left(errors.head)
          }

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

            case Left(l) => l
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
