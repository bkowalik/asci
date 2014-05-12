package com.asci

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