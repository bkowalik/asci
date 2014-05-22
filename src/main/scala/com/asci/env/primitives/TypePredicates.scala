package com.asci.env.primitives

import com.asci.Constant._
import com.asci.Constant.IntegerNum
import scala.Tuple1
import com.asci.Constant.BooleanConstant
import com.asci.Constant.FloatingNum
import com.asci.Expr.ListExpr

object TypePredicates {
  // copy-paste driven development

  def isOfType[A, B](a: Tuple1[A]): A = BooleanConstant(a._1 match {
    case foo: B => true
    case _    => false
  }).asInstanceOf[A]

  def number[A](a: Tuple1[A]): A = BooleanConstant(a._1 match {
    case foo: Num[_] => true
    case _           => false
  }).asInstanceOf[A]

  def real[A](a: Tuple1[A]): A = BooleanConstant(a._1 match {
    case foo: FloatingNum => true
    case _                => false
  }).asInstanceOf[A]

  def integer[A](a: Tuple1[A]): A = BooleanConstant(a._1 match {
    case foo: IntegerNum => true
    case _               => false
  }).asInstanceOf[A]

  def boolean[A](a: Tuple1[A]): A = BooleanConstant(a._1 match {
    case foo: BooleanConstant => true
    case _                    => false
  }).asInstanceOf[A]

  def char[A](a: Tuple1[A]): A = BooleanConstant(a._1 match {
    case foo: CharacterConstant => true
    case _                      => false
  }).asInstanceOf[A]

  def string[A](a: Tuple1[A]): A = BooleanConstant(a._1 match {
    case foo: StringConstant => true
    case _                   => false
  }).asInstanceOf[A]

  def list[A](a: Tuple1[A]): A = BooleanConstant(a._1 match {
    case foo: ListExpr => true
    case _             => false
  }).asInstanceOf[A]

}
