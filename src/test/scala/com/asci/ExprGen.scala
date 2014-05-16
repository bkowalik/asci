package com.asci

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import com.asci.Constant._
import com.asci.Expr.{DottedList, ListExpr, Atom}

object ExprGen {
  // FIXME: add quotation generator

  def expr: Gen[Expr] = gen(0)

  // FIXME: pass depth limit as parameter
  def gen(i: Int): Gen[Expr] = if (i > 2) leafs else lists(i + 1)

  def all(i: Int): Gen[Expr] = Gen.oneOf(leafs, lists(i))

  def leafs: Gen[Expr] = Gen.oneOf(int, float, atom, char, string, bool)

  def lists(i: Int): Gen[Expr] = Gen.oneOf(properList(i), dottedList(i))

  def atom: Gen[Atom] = GenHelpers.identifier flatMap Atom.apply

  def bool: Gen[BooleanConstant] = arbitrary[Boolean] flatMap BooleanConstant.apply

  lazy val niceChar: Arbitrary[Char] = Arbitrary(
    Gen.choose(32, 127)
  )

  def char: Gen[CharacterConstant] = niceChar.arbitrary flatMap CharacterConstant.apply

  lazy val niceString: Arbitrary[String] = Arbitrary(
    Gen.listOfN(10, Gen.alphaNumChar) map (_.mkString)
  )
  def string: Gen[StringConstant] = niceString.arbitrary flatMap StringConstant.apply

  def float: Gen[FloatingNum] = GenHelpers.simpleFloat flatMap FloatingNum.apply

  def int: Gen[IntegerNum] = Gen.choose(0, Int.MaxValue) flatMap IntegerNum.apply

  // FIXME: do not ignore size parameter
  def properList(i: Int): Gen[ListExpr] = Gen.sized { size =>
    Gen.listOfN(2, gen(i)) flatMap ListExpr.apply
  }

  def dottedList(i: Int): Gen[DottedList] = Gen.sized { size =>
    for {
      n <- Gen.listOfN(2, gen(i))
      v <- gen(i)
    } yield DottedList(n, v)
  }
}

object GenHelpers {

  def identifier: Gen[String] = Gen.sized { size =>
    for {
      init <- Gen.oneOf(letter, initialSymbol)
      rest <- Gen.listOfN(size % 10 + 1, Gen.oneOf(letter, digit, symbol))
    } yield init + rest.mkString
  }

  def letter: Gen[Char] = Gen.alphaChar

  def symbol: Gen[Char] = Gen.oneOf(initialSymbol, Gen.oneOf(".+-".toSeq))

  def initialSymbol: Gen[Char] = Gen.oneOf("!$%&*/:<=>?~_^".toSeq)

  def digit: Gen[Char] = Gen.numChar

  def simpleFloat: Gen[Float] = for {
    i <- Gen.nonEmptyListOf(digit)
    f <- Gen.nonEmptyListOf(digit)
  } yield {
    val integral = i.take(6).mkString
    val fractional = f.take(6).mkString
    s"$integral.$fractional".toFloat
  }
}