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
}

object ParserTest {
  sealed abstract class Expr

  case class Constant() extends Expr
  case class Num() extends Constant
  case class IntegerNum(i: Int) extends Num
  case class FloatingNum(f: Float) extends Num

  sealed abstract class Sign
  case object Plus extends Sign
  case object Minus extends Sign

  class Parser extends JavaTokenParsers {

    override val skipWhitespace = false

    def read(input: String): ParseResult[_] = parseAll(scheme, input)

    def scheme: Parser[_] = identifier

    def identifier: Parser[_] = (letter | symbol) ~ rep(letter | digit | symbol)

    def letter: Parser[Char] = oneOf("abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUWXYZ")

    def oneOf(a: String): Parser[Char] = (a.toList.tail map elem).foldLeft(elem(a.toList.head)) {case (acc, c) => acc | c}

    def symbol: Parser[Char] = oneOf("!$%&*/:<=>?~_^.+-")

    def digit: Parser[Char] = oneOf("0123456789")

    def floating: Parser[Float] = rep1(digit) ~ elem('.') ~ rep1(digit) ^^ {
      case foo ~ _ ~ bar => s"${foo.mkString}.${bar.mkString}".toFloat
    }

    def integer: Parser[Int] = rep1(digit) ^^ {
      case foo => foo.mkString.toInt
    }

    def boolean: Parser[Boolean] = elem('#') ~> oneOf("tf") ^^ {
      case 't' => true
      case 'f' => false
    }

    def sign: Parser[Sign] = oneOf("+-") ^^ {
      case '+' => Plus
      case '-' => Minus
    }

    def number: Parser[Num] = opt(sign) ~ (floating | integer) ^^ {
      case s ~ num => num match {
        case n: Float => ???
        case n: Int => ???
      }
    }
  }
}
