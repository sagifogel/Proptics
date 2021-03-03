package proptics.specs

import cats.data.{Chain, NonEmptyList, NonEmptyVector}

import proptics.instances.foldableWithIndex._
import proptics.law.discipline.FoldableWithIndexTests

class FoldableWithIndexSpec extends PropticsSuite {
  checkAll("FoldableWithIndex[Option, Unit]", FoldableWithIndexTests[Option, Unit].foldableWithIndex[Int, Int, Int])
  checkAll("FoldableWithIndex[Vector, Int]", FoldableWithIndexTests[Vector, Int].foldableWithIndex[Int, Int, Int])
  checkAll("FoldableWithIndex[List, Int]", FoldableWithIndexTests[List, Int].foldableWithIndex[Int, Int, Int])
  checkAll("FoldableWithIndex[Map[Int, *], Int]", FoldableWithIndexTests[Map[Int, *], Int].foldableWithIndex[Int, Int, Int])
  checkAll("FoldableWithIndex[Chain, Int]", FoldableWithIndexTests[Chain, Int].foldableWithIndex[Int, Int, Int])
  checkAll("FoldableWithIndex[NonEmptyVector, Int]", FoldableWithIndexTests[NonEmptyVector, Int].foldableWithIndex[Int, Int, Int])
  checkAll("FoldableWithIndex[NonEmptyList, Int]", FoldableWithIndexTests[NonEmptyList, Int].foldableWithIndex[Int, Int, Int])
}
