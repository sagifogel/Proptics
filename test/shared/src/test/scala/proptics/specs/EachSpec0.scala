package proptics.specs

import scala.collection.immutable.{ListMap, SortedMap}

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector, OneAnd}

import proptics.instances.each._
import proptics.law.discipline.EachTests

trait EachSpec0 extends PropticsSuite {
  checkAll("Each[String, Char] Each", EachTests[String, Char].each)
  checkAll("Each[List[Int], Int] Each", EachTests[List[Int], Int].each)
  checkAll("Each[Vector[Int], Int] Each", EachTests[Vector[Int], Int].each)
  checkAll("Each[ListMap[Int, Int], Int] Each", EachTests[ListMap[Int, Int], Int].each)
  checkAll("Each[SortedMap[Int, Int], Int] Each", EachTests[SortedMap[Int, Int], Int].each)
  checkAll("Each[Chain[Int], Int] Each", EachTests[Chain[Int], Int].each)
  checkAll("Each[OneAnd[List, Int], Int] Each", EachTests[OneAnd[List, Int], Int].each)
  checkAll("Each[NonEmptyVector[Int], Int] Each", EachTests[NonEmptyVector[Int], Int].each)
  checkAll("Each[NonEmptyList[Int], Int] Each", EachTests[NonEmptyList[Int], Int].each)
  checkAll("Each[NonEmptyChain[Int], Int] Each", EachTests[NonEmptyChain[Int], Int].each)
}
