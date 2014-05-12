package com.asci

import org.scalatest._
import scala.util.parsing.combinator._
import com.asci.ParserTest.Parser

class ParserTest extends FlatSpec with Matchers {
  behavior of "parser"

  trait ParserSupplier {
    val parser = new Parser
  }

  it should "parse identifier" in new ParserSupplier {
    parser.read("&&gf2tf3") shouldBe a [parser.Success[_]]
  }

  it should "parse letter" in new ParserSupplier {
    parser.parseAll(parser.letter, "f") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.letter, "h") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.letter, "D") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.letter, "3") should not be an [parser.Success[_]]
  }

  it should "parse digit" in new ParserSupplier {
    parser.parseAll(parser.digit, "5") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.digit, "f") should not be an [parser.Success[_]]
  }

  it should "parse symbol" in new ParserSupplier {
    parser.parseAll(parser.symbol, "%") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.symbol, "o") should not be an [parser.Success[_]]
  }

  it should "parse float" in new ParserSupplier {
    parser.parseAll(parser.floating, "2.45") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.floating, "5") should not be an [parser.Success[_]]
  }

  it should "parse integer" in new ParserSupplier {
    parser.parseAll(parser.integer, "5") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.integer, "foobar") should not be an [parser.Success[_]]
    parser.parseAll(parser.integer, "") should not be an [parser.Success[_]]
  }

  it should "parse boolean" in new ParserSupplier {
    parser.parseAll(parser.boolean, "#t") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.boolean, "#f") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.boolean, "#p") should not be an [parser.Success[_]]
    parser.parseAll(parser.boolean, "#3") should not be an [parser.Success[_]]
  }

  it should "parse sign" in new ParserSupplier {
    parser.parseAll(parser.sign, "+") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.sign, "]") should not be an [parser.Success[_]]
  }

  it should "parse num with sign" in new ParserSupplier {
    parser.parseAll(parser.number, "+2") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.number, "-5") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.number, "7") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.number, "+22.56") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.number, "-1.34") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.number, "2.88") shouldBe a [parser.Success[_]]
  }

  it should "parse string character" in new ParserSupplier {
    parser.parseAll(parser.stringLiteral, "\"\\\"\"") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.stringLiteral, "\"\\\\\"") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.stringLiteral, "\"f\"") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.stringLiteral, "\"6\"") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.stringLiteral, "\"*\"") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.stringLiteral, "\"\"\"") should not be an [parser.Success[_]]
    parser.parseAll(parser.stringLiteral, "\"\\\"") should not be an [parser.Success[_]]
  }

  it should "parse character" in new ParserSupplier {
    parser.parseAll(parser.character, "#\\s") shouldBe a [parser.Success[_]]
  }

  it should "parse constant" in new ParserSupplier {
    parser.parseAll(parser.constant, "9") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.constant, "#\\s") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.constant, "2.22") shouldBe a [parser.Success[_]]
  }

  it should "parse list" in new ParserSupplier {
    parser.parseAll(parser.list, "(\tdefine %%%  6)") shouldBe a [parser.Success[_]]
  }

  it should "parse dotted list" in new ParserSupplier {
    parser.parseAll(parser.dottedList, "(\t1 2     3 \t. 4)") shouldBe a [parser.Success[_]]
  }

  it should "parse quotation" in new ParserSupplier {
    parser.parseAll(parser.quotation, "'   \t1") shouldBe a [parser.Success[_]]
    parser.parseAll(parser.quotation, "' (1 2 3 4)") shouldBe a [parser.Success[_]]
  }

  it should "parse nested lists" in new ParserSupplier {
    parser.parseAll(parser.list, "(define x (if (= x 0) (3) (/ 20 4)))") shouldBe a [parser.Success[_]]
  }
}

object ParserTest {
  sealed abstract class Expr
  sealed abstract class Constant extends Expr

  sealed abstract class Num extends Constant
  case class IntegerNum(i: Int) extends Num
  case class FloatingNum(f: Float) extends Num

  case class StringConstant(str: String) extends Constant
  case class CharacterConstant(c: Char) extends Constant
  case class BooleanConstant(b: Boolean) extends Constant

  case class ListExpr(l: List[Expr]) extends Expr
  case class DottedList(l: List[Expr], e: Expr) extends Expr

  case class Quotation(q: Expr) extends Expr

  case class Atom(f: String) extends Expr

  sealed abstract class Sign
  case object Plus extends Sign
  case object Minus extends Sign

  class Parser extends JavaTokenParsers {

    override val skipWhitespace = false

    def read(input: String): ParseResult[Expr] = parseAll(scheme, input)

    def scheme: Parser[Expr] = identifier

    def identifier: Parser[Expr] = (letter | initialSymbol) ~ rep(letter | digit | symbol) ^^ {
      case foo ~ bar => Atom(s"$foo$bar")
    }

    def letter: Parser[Char] = oneOf("abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUWXYZ")

    def oneOf(a: String): Parser[Char] = (a.toList.tail map elem).foldLeft(elem(a.toList.head)) {case (acc, c) => acc | c}

    def symbol: Parser[Char] = initialSymbol | oneOf(".+-")

    def initialSymbol: Parser[Char] = oneOf("!$%&*/:<=>?~_^")

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

    def number: Parser[Num] = opt(sign) ~ (floating | integer) ^^ {
      case s ~ (num: Int) => IntegerNum(applySign(s, num))
      case s ~ (num: Float) => FloatingNum(applySign(s, num))
    }

    def character: Parser[CharacterConstant] = (elem('#') ~ elem('\\')) ~> ".".r ^^ {
      case str => CharacterConstant(str.charAt(0))
    }

    def constant: Parser[Constant] = boolean | number | character | string

    def string: Parser[StringConstant] = stringLiteral  ^^(StringConstant(_))

    private[asci] def applySign[T](sign: Option[Sign], n: T)(implicit num: Numeric[T]): T = {
      sign.map{
        case Plus => n
        case Minus => num.negate(n)
      } getOrElse(n)
    }

    def expression: Parser[Expr] = constant | identifier | list

    def list: Parser[ListExpr] = lexeme(elem('(')) ~> (lexeme(expression)).* <~ lexeme(elem(')')) ^^(ListExpr(_))

    private[asci] def lexeme[T](p: Parser[T]): Parser[T] = p <~ opt(whitespace)

    private[asci] def whitespace = """[ \t]+""".r

    def dottedList: Parser[DottedList] = lexeme(elem('(')) ~> rep1(lexeme(expression)) ~ lexeme(elem('.')) ~ lexeme(expression) <~ lexeme(elem(')')) ^^ {
      case exprs ~ _ ~ expr => DottedList(exprs, expr)
    }

    def quotation: Parser[Quotation] = lexeme(elem('\'')) ~> lexeme(expression) ^^(Quotation(_))
  }
}
