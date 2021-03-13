package proptics.specs

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector, OneAnd}

import proptics.instances.nonEmptyCons._
import proptics.law.discipline.NonEmptyConsTests

class NonEmptyCons extends PropticsSuite {
  checkAll("NonEmptyCons[NonEmptyVector[Int], Int, Vector[Int]] noEmptyCons", NonEmptyConsTests[NonEmptyVector[Int], Int, Vector[Int]].nonEmptyCons)
  checkAll("NonEmptyCons[NonEmptyList[Int], Int, List[Int]] noEmptyCons", NonEmptyConsTests[NonEmptyList[Int], Int, List[Int]].nonEmptyCons)
  checkAll("NonEmptyCons[NonEmptyChain[Int], Int, Chain[Int]] noEmptyCons", NonEmptyConsTests[NonEmptyChain[Int], Int, Chain[Int]].nonEmptyCons)
  checkAll("NonEmptyCons[OneAnd[List, Int], Int, List[Int]] noEmptyCons", NonEmptyConsTests[OneAnd[List, Int], Int, List[Int]].nonEmptyCons)
}
