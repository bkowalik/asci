package com.asci.env

import com.asci.Expr
import com.asci.Expr.{ExprFun, Fixed, Variable, FunWrap}
import com.asci.Constant.{FloatingNum, StringConstant, Num}
import scalaz.Monoid

case class Env(private val env: Map[String, Expr]) {
  def this() = this(Map.empty)

  def insert(name: String, expr: Expr): Env = copy(env = env.updated(name, expr))

  def remove(name: String): Env = copy(env = env - name)

  def get(name: String): Option[Expr] = env.get(name)
}

object Env {
  import primitives.Internal._
  import primitives.List._
  import primitives.Numeric._
  import primitives.TypePredicates._
  import primitives.Eq._

  lazy val r5rsEnv = Env(Map("+" -> FunWrap(add     [Num[Float], Float], Variable),
                             "-" -> FunWrap(subtract[Num[Float], Float], Variable),
                             "*" -> FunWrap(multiply[Num[Float], Float], Variable),
                             "/" -> FunWrap(divide  [Num[Float], Float], Variable),

                             // FIXME: should be varargs
                             "<"  -> FunWrap(<[Expr, Float], Fixed(2)),
                             "<=" -> FunWrap(<=[Expr, Float], Fixed(2)),
                             "="  -> FunWrap(=?[Expr, Float], Fixed(2)),
                             ">"  -> FunWrap(>[Expr, Float], Fixed(2)),
                             ">=" -> FunWrap(>=[Expr, Float], Fixed(2)),


                             "car"       -> FunWrap(car [Expr], Fixed(1)),
                             "cdr"       -> FunWrap(cdr [Expr], Fixed(1)),
                             "cons"      -> FunWrap(cons[Expr], Fixed(2)),
                             // FIXME: this should really default second argument to '()
                             // FIXME: not sure if this has to be a primitive
                             "make-list" -> FunWrap(makeList[Expr], Fixed(2)),

                             "define" -> ExprFun(define),
                             "if"     -> ExprFun(`if`),
                             "let"    -> ExprFun(let),
                             "lambda" -> ExprFun(lambda),

                             "number?"  -> FunWrap(number[Expr], Fixed(1)),
                             "real?"    -> FunWrap(real[Expr], Fixed(1)),
                             "integer?" -> FunWrap(integer[Expr], Fixed(1)),
                             "char?"    -> FunWrap(char[Expr], Fixed(1)),
                             "string?"  -> FunWrap(string[Expr], Fixed(1)),
                             "list?"    -> FunWrap(list[Expr], Fixed(1)),

                             // FIXME: these functions should work on any number of args from 0 to inf
                             // FIXME: not a correct behaviour, but maybe we can get away with it
                             "eq?"  -> FunWrap(equal[Expr], Fixed(2)),
                             "eqv?" -> FunWrap(equal[Expr], Fixed(2)),

                             // not really useful functions
                             "concat" -> FunWrap(concat[StringConstant], Variable),
                             "fst"    -> FunWrap(fst[Expr], Fixed(2)),
                             "snd"    -> FunWrap(snd[Expr], Fixed(2))
  ))

  implicit object EnvMonoid extends Monoid[Env] {
    def zero: Env = new Env()

    def append(e1: Env, e2: => Env): Env = e2.env.foldLeft(e1) {
      case (acc, elem) => e1.get(elem._1) match {
        case Some(_) => acc
        case None    => acc.insert(elem._1, elem._2)
      }
    }
  }
}