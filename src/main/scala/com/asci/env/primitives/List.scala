package com.asci.env.primitives

import com.asci.Expr.{DottedList, ListExpr}
import com.asci.{NotImplemented, TypeMismatch}
import com.asci.Constant.StringConstant

object List {
  def car[A](a: Tuple1[A]): A = a._1 match {
    case ListExpr(head :: _) => head.asInstanceOf[A]
    case DottedList(head :: _, _) => head.asInstanceOf[A]
    case _ => throw TypeMismatch("non-empty list", a.toString())
  }

  def cdr[A](a: Tuple1[A]): A = throw NotImplemented("cdr")

  def cons[A](a: (A, A)): A = throw NotImplemented("cons")

  def concat[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => StringConstant(s1 + s2).asInstanceOf[A]
  }
}
