package com.asci

import scalaz.Show

sealed abstract class EvalError extends Throwable

case class InvalidArgsNumber(valid: Int)               extends EvalError
case class DivideByZero()                              extends EvalError
case class UnboundVariable(name: String)               extends EvalError
case class TypeMismatch(expected: String, got: String) extends EvalError
case class NotImplemented(name: String)                extends EvalError
case class ParseError(err: String)                     extends EvalError
case class OtherError(name: String)                    extends EvalError

object EvalError {
  implicit object ShowEvalError extends Show[EvalError] {
    override def shows(e: EvalError): String = e match {
      case InvalidArgsNumber(valid) => s"Invalid number of arguments passed, required $valid"
      case UnboundVariable(a)       => s"Unbound variable $a"
      case TypeMismatch(ex, g)      => s"Type mismatch, expected $ex, got $g"
      case NotImplemented(f)        => s"Feature $f is not yet implemented"
      case ParseError(err)          => s"Parse error: $err"
      case OtherError(err)          => err
    }
  }
}