package com.asci

import com.asci.Constant.Num

object Primitives {

  // Numeric primitives

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
