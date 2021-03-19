package proptics.specs

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import org.scalacheck.Cogen

import proptics.instances.reverse._
import proptics.law.discipline.ReverseTests

private[specs] trait ReverseSpec0 extends PropticsSuite {
  implicit val cogenNev: Cogen[NonEmptyVector[Int]] = Cogen.it(_.iterator)
  implicit val cogenNel: Cogen[NonEmptyList[Int]] = Cogen.it(_.iterator)
  implicit val cogenNec: Cogen[NonEmptyChain[Int]] = Cogen.it(_.iterator)

  checkAll("ReverseTests[String, String] reverse", ReverseTests[String, String].reverse)
  checkAll("ReverseTests[Array[Int], Array[Int]] reverse", ReverseTests[Array[Int], Array[Int]].reverse)
  checkAll("ReverseTests[Vector[Int], Vector[Int]] reverse", ReverseTests[Vector[Int], Vector[Int]].reverse)
  checkAll("ReverseTests[List[Int], List[Int]] reverse", ReverseTests[List[Int], List[Int]].reverse)
  checkAll("ReverseTests[(Int, Int),(Int, Int)] reverse", ReverseTests[(Int, Int), (Int, Int)].reverse)
  checkAll("ReverseTests[(Int, Int, Int), (Int, Int, Int)] reverse", ReverseTests[(Int, Int, Int), (Int, Int, Int)].reverse)
  checkAll("ReverseTests[(Int, Int, Int, Int), (Int, Int, Int, Int)] reverse", ReverseTests[(Int, Int, Int, Int), (Int, Int, Int, Int)].reverse)
  checkAll("ReverseTests[(Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int)] reverse", ReverseTests[(Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int)].reverse)
  checkAll("ReverseTests[Chain[Int], Chain[Int]] reverse", ReverseTests[Chain[Int], Chain[Int]].reverse)
  checkAll("ReverseTests[NonEmptyVector[Int], NonEmptyVector[Int]] reverse", ReverseTests[NonEmptyVector[Int], NonEmptyVector[Int]].reverse)
  checkAll("ReverseTests[NonEmptyList[Int], NonEmptyList[Int]] reverse", ReverseTests[NonEmptyList[Int], NonEmptyList[Int]].reverse)
  checkAll("ReverseTests[NonEmptyChain[Int], NonEmptyChain[Int]] reverse", ReverseTests[NonEmptyChain[Int], NonEmptyChain[Int]].reverse)
}
