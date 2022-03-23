package proptics.specs

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector, OneAnd}

import proptics.instances.prefixed._
import proptics.law.discipline.PrefixedTests

private[specs] trait PrefixedSpec0 extends PropticsSuite {
  checkAll("PrefixedTests[String, String] prefix", PrefixedTests[String, String].prefixed)
  checkAll("PrefixedTests[Array[Int], Array[Int]] prefix", PrefixedTests[Array[Int], Array[Int]].prefixed)
  checkAll("PrefixedTests[Vector[Int], Vector[Int]] prefix", PrefixedTests[Vector[Int], Vector[Int]].prefixed)
  checkAll("PrefixedTests[List[Int], List[Int]] prefix", PrefixedTests[List[Int], List[Int]].prefixed)
  checkAll("PrefixedTests[Chain[Int], Chain[Int]] prefix", PrefixedTests[Chain[Int], Chain[Int]].prefixed)
  checkAll("PrefixedTests[NonEmptyVector[Int], Vector[Int]] prefix", PrefixedTests[NonEmptyVector[Int], Vector[Int]].prefixed)
  checkAll("PrefixedTests[NonEmptyList[Int], List[Int]] prefix", PrefixedTests[NonEmptyList[Int], List[Int]].prefixed)
  checkAll("PrefixedTests[NonEmptyChain[Int], Chain[Int]] prefix", PrefixedTests[NonEmptyChain[Int], Chain[Int]].prefixed)
  checkAll("PrefixedTests[OneAnd[List, Int], List[Int]] prefix", PrefixedTests[OneAnd[List, Int], List[Int]].prefixed)
}
