package proptics.specs

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector, OneAnd}

import proptics.instances.prefixed._
import proptics.law.discipline.PrefixedTests

private[specs] trait PrefixedSpec0 extends PropticsSuite {
  checkAll("PrefixedTests[String, String] prefixed", PrefixedTests[String, String].prefixed)
  checkAll("PrefixedTests[Array[Int], Array[Int]] prefixed", PrefixedTests[Array[Int], Array[Int]].prefixed)
  checkAll("PrefixedTests[Vector[Int], Vector[Int]] prefixed", PrefixedTests[Vector[Int], Vector[Int]].prefixed)
  checkAll("PrefixedTests[List[Int], List[Int]] prefixed", PrefixedTests[List[Int], List[Int]].prefixed)
  checkAll("PrefixedTests[Chain[Int], Chain[Int]] prefixed", PrefixedTests[Chain[Int], Chain[Int]].prefixed)
  checkAll("PrefixedTests[NonEmptyVector[Int], Vector[Int]] prefixed", PrefixedTests[NonEmptyVector[Int], Vector[Int]].prefixed)
  checkAll("PrefixedTests[NonEmptyList[Int], List[Int]] prefixed", PrefixedTests[NonEmptyList[Int], List[Int]].prefixed)
  checkAll("PrefixedTests[NonEmptyChain[Int], Chain[Int]] prefixed", PrefixedTests[NonEmptyChain[Int], Chain[Int]].prefixed)
  checkAll("PrefixedTests[OneAnd[List, Int], List[Int]] prefixed", PrefixedTests[OneAnd[List, Int], List[Int]].prefixed)
}
