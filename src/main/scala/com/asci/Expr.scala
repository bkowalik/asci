package com.asci

import scalaz._

sealed abstract class Expr

object Expr {
  case class ListExpr(l: List[Expr]) extends Expr
  case class DottedList(l: List[Expr], e: Expr) extends Expr
  case class Quotation(q: Expr) extends Expr
  case class Atom(f: String) extends Expr
}

sealed abstract class Constant extends Expr

object Constant {
  //TODO: fix runtime strict typing and uncomment proper comments
  sealed abstract class Num[+T]()(implicit num: Numeric[T]) extends Constant{
    val value: T
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

object ShowExpr extends Show[Expr] {
  import Expr._
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