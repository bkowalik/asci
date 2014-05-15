package com.asci

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import com.asci.Constant._
import com.asci.Expr.{DottedList, ListExpr, Atom}

object ExprGen {

  def expr: Gen[Expr] = gen(0)

  // FIXME: pass depth limit as parameter
  def gen(i: Int): Gen[Expr] = if (i > 3) leafs else lists(i + 1)

  def all(i: Int): Gen[Expr] = Gen.oneOf(leafs, lists(i))

  def leafs: Gen[Expr] = Gen.oneOf(int, float, atom, char, string, bool)

  def lists(i: Int): Gen[Expr] = Gen.oneOf(properList(i), dottedList(i))

  // FIXME: forbid generating invalid identifiers
  def atom: Gen[Atom] = niceString.arbitrary flatMap Atom.apply

  def bool: Gen[BooleanConstant] = arbitrary[Boolean] flatMap BooleanConstant.apply

  lazy val niceChar: Arbitrary[Char] = Arbitrary(
    Gen.choose(Char.MinValue, 127)
  )

  def char: Gen[CharacterConstant] = niceChar.arbitrary flatMap CharacterConstant.apply

  lazy val niceString: Arbitrary[String] = Arbitrary(
    Gen.listOf(niceChar.arbitrary) map (_.mkString)
  )
  def string: Gen[StringConstant] = niceString.arbitrary flatMap StringConstant.apply

  def float: Gen[FloatingNum] = arbitrary[Float] flatMap FloatingNum.apply

  def int: Gen[IntegerNum] = arbitrary[Int] flatMap IntegerNum.apply

  // FIXME: do not ignore size parameter
  def properList(i: Int): Gen[ListExpr] = Gen.sized { size =>
    Gen.listOfN(3, gen(i)) flatMap ListExpr.apply
  }

  def dottedList(i: Int): Gen[DottedList] = Gen.sized { size =>
    for {
      n <- Gen.listOfN(3, gen(i))
      v <- gen(i)
    } yield DottedList(n, v)
  }
}
