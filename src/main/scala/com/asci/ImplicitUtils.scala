package com.asci

object ImplicitUtils {
  implicit class Sequencable[A,B](val foo: List[Either[A,B]]) {
    def sequence: Either[A, List[B]] = foo.partition(_.isLeft) match {
      case (Nil, rights) => Right(for(Right(r) <- rights) yield r)
      case (Left(err) :: _, _) => Left(err)
    }
  }
}
