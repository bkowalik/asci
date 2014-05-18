package com.asci

import scalaz._
import com.asci.env.Env

sealed abstract class Expr

object Expr {
  import Constant._

  case class ListExpr(l: List[Expr]) extends Expr
  case class DottedList(l: List[Expr], e: Expr) extends Expr
  case class Quotation(q: Expr) extends Expr
  case class Atom(f: String) extends Expr
  case class ExprFun(f: (Env, List[Expr]) => Either[EvalError, (Env, Expr)]) extends Expr
  case class FunWrap[A, B](f: Function[B, A], arity: Arity) extends Expr
  case class Lambda(formals: Expr, body: Expr, closure: Env) extends Expr

  sealed abstract class Arity
  case class  Fixed(i: Int) extends Arity
  case object Variable      extends Arity

  def apply[A](a: A): Expr = a match {
    case e: Expr => e
    case s: String => StringConstant(s)
  }

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
      case Lambda(f, b, _)      => "(lambda " + shows(f) + " " + shows(b) + ")"
    }
  }
}

sealed abstract class Constant extends Expr

object Constant {
  //TODO: fix runtime strict typing and uncomment proper comments
  sealed abstract class Num[+T : Numeric]() extends Constant {
    val value: T
  }

  object Num {
    def +[T : Numeric](a: Num[T], b: Num[T]): Num[T] = a match {
      case IntegerNum(v1) => b match {
        case IntegerNum(v2) => IntegerNum(v1 + v2).asInstanceOf[Num[T]]
        case FloatingNum(v2) => FloatingNum(v1 + v2).asInstanceOf[Num[T]]
      }
      case FloatingNum(v1) => FloatingNum(v1 + float(b)).asInstanceOf[Num[T]]
    }

    def -[T : Numeric](a: Num[T], b: Num[T]): Num[T] = a match {
      case IntegerNum(v1) => b match {
        case IntegerNum(v2) => IntegerNum(v1 - v2).asInstanceOf[Num[T]]
        case FloatingNum(v2) => FloatingNum(v1 - v2).asInstanceOf[Num[T]]
      }
      case FloatingNum(v1) => FloatingNum(v1 - float(b)).asInstanceOf[Num[T]]
    }

    def *[T : Numeric](a: Num[T], b: Num[T]): Num[T] = a match {
      case IntegerNum(v1) => b match {
        case IntegerNum(v2) => IntegerNum(v1 * v2).asInstanceOf[Num[T]]
        case FloatingNum(v2) => FloatingNum(v1 * v2).asInstanceOf[Num[T]]
      }
      case FloatingNum(v1) => FloatingNum(v1 * float(b)).asInstanceOf[Num[T]]
    }

    def /[T : Numeric](a: Num[T], b: Num[T]): Num[T] = a match {
      case IntegerNum(v1) => b match {
        case IntegerNum(v2) => IntegerNum(v1 / v2).asInstanceOf[Num[T]]
        case FloatingNum(v2) => FloatingNum(v1 / v2).asInstanceOf[Num[T]]
      }
      case FloatingNum(v1) => FloatingNum(v1 / float(b)).asInstanceOf[Num[T]]
    }

    def float[T : Numeric](a: Num[T]): Float = a match {
      case IntegerNum(v) => v.toFloat
      case FloatingNum(v) => v
    }
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
