package com.asci

import scala.util.parsing.combinator.JavaTokenParsers
import com.asci.Expr.{Quotation, DottedList, ListExpr, Atom}
import com.asci.Constant._
import com.asci.Constant.FloatingNum
import com.asci.Constant.BooleanConstant
import com.asci.Constant.IntegerNum

class Parser extends JavaTokenParsers {

  override val skipWhitespace = false

  def read(input: String): ParseResult[List[Expr]] = parseAll(scheme, input)

  def scheme: Parser[List[Expr]] = rep(expression)

  def identifier: Parser[Expr] = (letter | initialSymbol) ~ rep(letter | digit | symbol) ^^ {
    case foo ~ bar => Atom(bar.mkString.+:(foo))
  }

  def letter: Parser[Char] = oneOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

  def oneOf(a: String): Parser[Char] = (a.toList.tail map elem).foldLeft(elem(a.toList.head)) {case (acc, c) => acc | c}

  def symbol: Parser[Char] = initialSymbol | oneOf(".")

  def initialSymbol: Parser[Char] = oneOf("!$%&*/:<=>?~_^+-")

  def digit: Parser[Char] = oneOf("0123456789")

  def floating: Parser[Float] = rep1(digit) ~ elem('.') ~ rep1(digit) ^^ {
    case foo ~ _ ~ bar => s"${foo.mkString}.${bar.mkString}".toFloat
  }

  def integer: Parser[Int] = rep1(digit) ^^ {
    case foo => foo.mkString.toInt
  }

  def boolean: Parser[BooleanConstant] = elem('#') ~> oneOf("tf") ^^ {
    case 't' => BooleanConstant(true)
    case 'f' => BooleanConstant(false)
  }

  def sign: Parser[Sign] = oneOf("+-") ^^ {
    case '+' => Plus
    case '-' => Minus
  }

  def number: Parser[Num[AnyVal]] = opt(sign) ~ (floating | integer) ^^ {
    case s ~ (num: Int) => IntegerNum(applySign(s, num))
    case s ~ (num: Float) => FloatingNum(applySign(s, num))
  }

  def character: Parser[CharacterConstant] = (elem('#') ~ elem('\\')) ~> ".".r ^^ {
    case str => CharacterConstant(str.charAt(0))
  }

  def constant: Parser[Constant] = boolean | number | character | string

  def string: Parser[StringConstant] = stringLiteral ^^ { s =>
    val foo = s.substring(1, s.size - 1) // omit ""
    StringConstant(foo)
  }

  private[asci] def applySign[T](sign: Option[Sign], n: T)(implicit num: Numeric[T]): T = {
    sign.map{
      case Plus => n
      case Minus => num.negate(n)
    } getOrElse n
  }

  def expression: Parser[Expr] = quotation | constant | identifier | dottedList | list

  def list: Parser[ListExpr] = lexeme(elem('(')) ~> lexeme(expression).* <~ lexeme(elem(')')) ^^ListExpr

  private[asci] def lexeme[T](p: Parser[T]): Parser[T] = p <~ opt(whitespace)

  private[asci] def whitespace = """[ \t]+""".r

  def dottedList: Parser[DottedList] = lexeme(elem('(')) ~> rep1(lexeme(expression)) ~ elem('.') ~ whitespace ~ lexeme(expression) <~ lexeme(elem(')')) ^^ {
    case exprs ~ _ ~ _ ~ expr => DottedList(exprs, expr)
  }

  def quotation: Parser[Quotation] = lexeme(elem('\'')) ~> lexeme(expression) ^^Quotation
}
