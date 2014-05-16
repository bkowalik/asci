package com.asci

import scalaz._

sealed abstract class Expr

object Expr {
  case class ListExpr(l: List[Expr]) extends Expr
  case class DottedList(l: List[Expr], e: Expr) extends Expr
  case class Quotation(q: Expr) extends Expr
  case class Atom(f: String) extends Expr
  case class ExprFun(f: (Env, List[Expr]) => Either[EvalError, (Env, Expr)]) extends Expr

  implicit object ShowExpr extends Show[Expr] {
    import Constant._
    override def shows(e: Expr): String = e match {
      case ListExpr(l)          => "(" + l.map(shows).mkString(" ") + ")"
      case DottedList(l, e)     => "(" + l.map(shows).mkString(" ") + " . " + shows(e) + ")"
      case Quotation(e)         => "'" + shows(e)
      case Atom(s)              => s
      case IntegerNum(i)        => i.toString
      case FloatingNum(f)       => f.toString
      case StringConstant(s)    => "\"" + s + "\""
      case CharacterConstant(c) => "#\\" + c
      case BooleanConstant(b)   => if (b) "#t" else "#f"
    }
  }
}

sealed abstract class Constant extends Expr

object Constant {
  //TODO: fix runtime strict typing and uncomment proper comments
  sealed abstract class Num[+T]() extends Constant {
    val value: T
  }

  object Num {
    def +[T](a: Num[T], b: Num[T])(implicit f: Numeric[T]): Num[T] = Num[T]{f.plus(a.value, b.value)}

    def apply[T](a: T): Num[T] = new Num[T] {override val value = a}

    def -[T](a: Num[T], b: Num[T])(implicit f: Numeric[T]): Num[T] = Num[T]{f.minus(a.value, b.value)}
  }

  case class IntegerNum(value: Int) extends Num[Int]
  case class FloatingNum(value: Float) extends Num[Float]

  case class StringConstant(str: String) extends Constant
  case class CharacterConstant(c: Char) extends Constant
  case class BooleanConstant(b: Boolean) extends Constant
}

sealed abstract class Sign
case object Plus extends Sign
case object Minus extends Sign
