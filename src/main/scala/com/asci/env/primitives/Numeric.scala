package com.asci.env.primitives

import com.asci.Constant.Num
import com.asci.Expr

object Numeric {
  def add[A, T](a: (A, A))(implicit f: Numeric[T]): A = a match {
    case (b: Num[T], c: Num[T]) => Num.+(b, c).asInstanceOf[A]
  }

  def subtract[A, T](a: (A, A))(implicit f: Numeric[T]): A = a match {
    case (b: Num[T], c: Num[T]) => Num.-(b, c).asInstanceOf[A]
  }

  def multiply[A, T](a: (A, A))(implicit f: Numeric[T]): A = a match {
    case (b: Num[T], c: Num[T]) => Num.*(b, c).asInstanceOf[A]
  }

  def divide[A, T](a: (A, A))(implicit f: Numeric[T]): A = a match {
    case (b: Num[T], c: Num[T]) => Num./(b, c).asInstanceOf[A]
  }

  def <[A, T](a: (A, A))(implicit f: Numeric[T]): A = a match {
    case (b: Num[T], c: Num[T]) => Expr(f.lt(Num.float(b).asInstanceOf[T], Num.float(c).asInstanceOf[T])).asInstanceOf[A]
  }
}
