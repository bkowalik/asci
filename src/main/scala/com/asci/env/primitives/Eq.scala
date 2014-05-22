package com.asci.env.primitives

import com.asci.Constant.BooleanConstant

object Eq {

  def equal[A](a: (A, A)): A = BooleanConstant(a match {
    case (foo, bar) => foo == bar
  }).asInstanceOf[A]

}
