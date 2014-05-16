package com.asci

sealed abstract class EvalError

case class InvalidArgsNumber(valid: Int)               extends EvalError
case class DivideByZero()                              extends EvalError
case class UnboundVariable(name: String)               extends EvalError
case class TypeMismatch(expected: String, got: String) extends EvalError
case class NotImplemented(name: String)                extends EvalError
case class ParseError(err: String)                     extends EvalError
case class OtherError(name: String)                    extends EvalError