package com.asci.env.primitives

import com.asci.Expr.{DottedList, ListExpr}
import com.asci.{Expr, TypeMismatch}
import com.asci.Constant.{IntegerNum, StringConstant}

object List {
  def car[A](a: Tuple1[A]): A = a._1 match {
    case ListExpr(head :: _) => head.asInstanceOf[A]
    case DottedList(head :: _, _) => head.asInstanceOf[A]
    case _ => throw TypeMismatch("non-empty list", a.toString())
  }

  def cdr[A](a: Tuple1[A]): A = a._1 match {
    case ListExpr(_ :: tail) => ListExpr(tail).asInstanceOf[A]
    case DottedList(_ :: Nil, e) => e.asInstanceOf[A]
    case DottedList(_ :: tail, e) => DottedList(tail, e).asInstanceOf[A]
    case _ => throw TypeMismatch("non-empty list", a.toString())
  }

  def cons[A](a: (A, A)): A = a match {
    case (foo, ListExpr(l)) => ListExpr(foo.asInstanceOf[Expr] :: l).asInstanceOf[A]
    case (foo, DottedList(l, e)) => DottedList(foo.asInstanceOf[Expr] :: l, e).asInstanceOf[A]
    case (foo, bar) => DottedList(scala.List(foo.asInstanceOf[Expr]), bar.asInstanceOf[Expr]).asInstanceOf[A]
  }

  def makeList[A](a: (A, A)): A = a match {
    case (IntegerNum(i), foo) => ListExpr(scala.List.fill(i)(foo.asInstanceOf[Expr])).asInstanceOf[A]
  }

  def concat[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => StringConstant(s1 + s2).asInstanceOf[A]
  }
}
