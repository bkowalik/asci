package com.asci

import org.scalacheck._
import com.asci.Expr._
import com.asci.Constant._

object ParserSpecification extends Properties("Parser") {
  import Prop.forAll
  import com.asci.ExprGen.expr

  implicit val gen: Arbitrary[Expr] = Arbitrary(expr)

  property("parse . show = id") = forAll { (e: Expr) =>
    val parser = new Parser
    val pretty = ShowExpr.shows(e)
    parser.read(pretty).getOrElse(List(ListExpr(List()))).head == e
  }

  def g: Expr => Stream[Expr] = {
    case a@ListExpr(l) => shrinkListExpr.shrink(a)
    case a@DottedList(l, e) => shrinkDottedList.shrink(a)
    case Quotation(q) => Stream.Empty
    case a@Atom(f) => shrinkAtom.shrink(a)
    case a: Constant => a match {
      case b: Num[_] => b match {
        case IntegerNum(value) => Shrink.shrinkInt.shrink(value) map IntegerNum
        case d@FloatingNum(value) => Stream(d)
      }
      case StringConstant(str) => {
        val s::ss = str.toList
        Shrink.shrinkContainer[List, Char].shrink(ss) map (foo => StringConstant((s::foo).mkString))
      }
      case d@CharacterConstant(c) => Stream(d)
      case d@BooleanConstant(b) => Stream(d)
    }
  }

  implicit lazy val shrinkExpr: Shrink[Expr] = Shrink {g}

  implicit def shrinkListExpr(implicit s1: Shrink[List[Expr]]): Shrink[ListExpr] = Shrink {
    case ListExpr(l :: ls) => for {l1 <- s1.shrink(ls)} yield ListExpr(l :: l1)
  }

  implicit def shrinkAtom(implicit s1: Shrink[String]): Shrink[Atom] = Shrink {
    case Atom(l) => for (l1 <- s1.shrink(l)) yield Atom(l1)
  }

  implicit def shrinkDottedList(implicit s1: Shrink[List[Expr]], s2: Shrink[Expr]): Shrink[DottedList] = Shrink {
    case DottedList(l :: ls, e) => (for {l1 <- s1.shrink(ls)} yield DottedList(l :: l1, e)) append (
      for {l2 <- s2.shrink(e)} yield DottedList(l::ls, l2)
      )
  }
}
