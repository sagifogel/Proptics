package proptics.specs

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector, OneAnd}

import proptics.instances.nonEmptyCons._
import proptics.law.discipline.NonEmptyConsTests

class NonEmptyCons extends PropticsSuite {
  checkAll("NonEmptyCons[NonEmptyVector[Int], Int, Vector[Int]] noEmptyCons", NonEmptyConsTests[NonEmptyVector[Int], Int, Vector[Int]].nonEmptyCons)
  checkAll("NonEmptyCons[NonEmptyList[Int], Int, List[Int]] noEmptyCons", NonEmptyConsTests[NonEmptyList[Int], Int, List[Int]].nonEmptyCons)
  checkAll("NonEmptyCons[NonEmptyChain[Int], Int, Chain[Int]] noEmptyCons", NonEmptyConsTests[NonEmptyChain[Int], Int, Chain[Int]].nonEmptyCons)
  checkAll("NonEmptyCons[OneAnd[List, Int], Int, List[Int]] noEmptyCons", NonEmptyConsTests[OneAnd[List, Int], Int, List[Int]].nonEmptyCons)

  checkAll("NonEmptyCons[NonEmptyVector[Int], Int, Vector[Int]] head", NonEmptyConsTests[NonEmptyVector[Int], Int, Vector[Int]].head)
  checkAll("NonEmptyCons[NonEmptyList[Int], Int, List[Int]] head", NonEmptyConsTests[NonEmptyList[Int], Int, List[Int]].head)
  checkAll("NonEmptyCons[NonEmptyChain[Int], Int, Chain[Int]] head", NonEmptyConsTests[NonEmptyChain[Int], Int, Chain[Int]].head)
  checkAll("NonEmptyCons[OneAnd[List, Int], Int, List[Int]] head", NonEmptyConsTests[OneAnd[List, Int], Int, List[Int]].head)

  checkAll("NonEmptyCons[NonEmptyVector[Int], Int, Vector[Int]] tail", NonEmptyConsTests[NonEmptyVector[Int], Int, Vector[Int]].tail)
  checkAll("NonEmptyCons[NonEmptyList[Int], Int, List[Int]] tail", NonEmptyConsTests[NonEmptyList[Int], Int, List[Int]].tail)
  checkAll("NonEmptyCons[NonEmptyChain[Int], Int, Chain[Int]] tail", NonEmptyConsTests[NonEmptyChain[Int], Int, Chain[Int]].tail)
  checkAll("NonEmptyCons[OneAnd[List, Int], Int, List[Int]] tail", NonEmptyConsTests[OneAnd[List, Int], Int, List[Int]].tail)
}
