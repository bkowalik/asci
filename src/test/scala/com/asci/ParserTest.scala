package com.asci

import org.scalatest._
import com.asci.Constant.IntegerNum
import com.asci.Expr.{ListExpr, Quotation, Atom}

class ParserTest extends FlatSpec with Matchers {
  behavior of "parser"

  trait ParserSupplier {
    val parser = new Parser
  }

  it should "parse identifier" in new ParserSupplier {
    val ident = parser.read("&&gf2tf3")
    ident shouldBe a [parser.Success[_]]
//    ident.get should equal("&&gf2tf3")
  }

  it should "parse letter" in new ParserSupplier {
    val letter_f = parser.parseAll(parser.letter, "f")
    letter_f shouldBe a [parser.Success[_]]
    letter_f.get should equal('f')

    val letter_h = parser.parseAll(parser.letter, "h")
    letter_h shouldBe a [parser.Success[_]]
    letter_h.get should equal('h')

    val letter_D = parser.parseAll(parser.letter, "D")
    letter_D shouldBe a [parser.Success[_]]
    letter_D.get should equal('D')

    parser.parseAll(parser.letter, "3") should not be an [parser.Success[_]]
  }

  it should "parse digit" in new ParserSupplier {
    val digit_5 = parser.parseAll(parser.digit, "5")
    digit_5 shouldBe a [parser.Success[_]]
    digit_5.get should equal('5')

    parser.parseAll(parser.digit, "f") should not be an [parser.Success[_]]
  }

  it should "parse symbol" in new ParserSupplier {
    val symbol_percentage = parser.parseAll(parser.symbol, "%")
    symbol_percentage shouldBe a [parser.Success[_]]
    symbol_percentage.get should equal('%')

    parser.parseAll(parser.symbol, "o") should not be an [parser.Success[_]]
  }

  it should "parse float" in new ParserSupplier {
    val number = parser.parseAll(parser.floating, "2.45")
    number shouldBe a [parser.Success[_]]
    number.get should equal(2.45f)

    parser.parseAll(parser.floating, "5") should not be an [parser.Success[_]]
  }

  it should "parse integer" in new ParserSupplier {
    val integer = parser.parseAll(parser.integer, "5")
    integer shouldBe a [parser.Success[_]]
    integer.get should equal(5)

    parser.parseAll(parser.integer, "foobar") should not be an [parser.Success[_]]
    parser.parseAll(parser.integer, "") should not be an [parser.Success[_]]
  }

  it should "parse boolean" in new ParserSupplier {
    val tr = parser.parseAll(parser.boolean, "#t")
    tr shouldBe a [parser.Success[_]]
    tr.get.b should equal(true)

    val fa = parser.parseAll(parser.boolean, "#f")
    fa shouldBe a [parser.Success[_]]
    fa.get.b should equal(false)

    parser.parseAll(parser.boolean, "#p") should not be an [parser.Success[_]]
    parser.parseAll(parser.boolean, "#3") should not be an [parser.Success[_]]
  }

  it should "parse sign" in new ParserSupplier {
    val plus_sign = parser.parseAll(parser.sign, "+")
    plus_sign shouldBe a [parser.Success[_]]
    plus_sign.get should be(Plus)

    parser.parseAll(parser.sign, "]") should not be an [parser.Success[_]]
  }

  it should "parse num with sign" in new ParserSupplier {
    val first = parser.parseAll(parser.number, "+2")
    first shouldBe a [parser.Success[IntegerNum]]
    first.get.value shouldBe a [Int]
    first.get.value should equal(2)

    val second = parser.parseAll(parser.number, "-5")
    second shouldBe a [parser.Success[_]]
    second.get.value should equal(-5)

    val third = parser.parseAll(parser.number, "7")
    third shouldBe a [parser.Success[_]]
    third.get.value should equal(7)

    val fourth = parser.parseAll(parser.number, "+22.56")
    fourth shouldBe a [parser.Success[_]]
    fourth.get.value should equal(22.56f)

    val fifth = parser.parseAll(parser.number, "-1.34")
    fifth shouldBe a [parser.Success[_]]
    fifth.get.value should equal(-1.34f)

    val sixth = parser.parseAll(parser.number, "2.88")
    sixth shouldBe a [parser.Success[_]]
    sixth.get.value should equal(2.88f)

    parser.parseAll(parser.number, "+as") should not be an [parser.Success[_]]
    parser.parseAll(parser.number, "-as.dd") should not be an [parser.Success[_]]
    parser.parseAll(parser.number, "-as.9") should not be an [parser.Success[_]]
    parser.parseAll(parser.number, "2as.9") should not be an [parser.Success[_]]
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
    val chr = parser.parseAll(parser.character, "#\\s")
    chr shouldBe a [parser.Success[_]]
    chr.get.c should equal('s')
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

  it should "parse dotted in proper" in new ParserSupplier {
    parser.read("((#f . 371716386))") shouldBe a [parser.Success[_]]
    parser.read("((1 . (a<c< . \"t\")))") shouldBe a [parser.Success[_]]
    parser.read("((/r . 954462.6) . (825951.06 . vv))") shouldBe a [parser.Success[_]]
  }

  it should "parse real-world example" in new ParserSupplier {
    parser.read("(+ 2 2)") shouldBe a [parser.Success[_]]
  }

  it should "properly parse quotations" in new ParserSupplier {
    val result = parser.read("'+")
    result.get.head should equal(Quotation(Atom("+")))

    val result2 = parser.read("(atom? 'turkey)")
    result2.get.head should equal(ListExpr(List(Atom("atom?"), Quotation(Atom("turkey")))))
  }
}
