package com.asci.env.primitives

import com.asci.Constant.Num

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
}
