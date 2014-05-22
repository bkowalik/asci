package com.asci.env.primitives

import com.asci.Constant.StringConstant
import com.asci.Expr

object String {

  def =?[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1 == s2).asInstanceOf[A]
  }

  def <[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1 < s2).asInstanceOf[A]
  }

  def <=[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1 <= s2).asInstanceOf[A]
  }

  def >[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1 > s2).asInstanceOf[A]
  }

  def >=[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1 >= s2).asInstanceOf[A]
  }

  def =?#[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1.toLowerCase == s2.toLowerCase).asInstanceOf[A]
  }

  def <#[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1.toLowerCase < s2.toLowerCase).asInstanceOf[A]
  }

  def <=#[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1.toLowerCase <= s2.toLowerCase).asInstanceOf[A]
  }

  def >#[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1.toLowerCase > s2.toLowerCase).asInstanceOf[A]
  }

  def >=#[A](a: (A, A)): A = a match {
    case (StringConstant(s1), StringConstant(s2)) => Expr(s1.toLowerCase >= s2.toLowerCase).asInstanceOf[A]
  }

}
