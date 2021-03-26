package proptics.specs

import cats.data._

import proptics.instances.suffixed._
import proptics.law.discipline.SuffixedTests

private[specs] trait SuffixedSpec0 extends PropticsSuite {
  checkAll("SuffixedTests[String, String] suffixed", SuffixedTests[String, String].suffixed)
  checkAll("SuffixedTests[Array[Int], Array[Int]] suffixed", SuffixedTests[Array[Int], Array[Int]].suffixed)
  checkAll("SuffixedTests[Vector[Int], Vector[Int]] suffixed", SuffixedTests[Vector[Int], Vector[Int]].suffixed)
  checkAll("SuffixedTests[List[Int], List[Int]] suffixed", SuffixedTests[List[Int], List[Int]].suffixed)
  checkAll("SuffixedTests[Chain[Int], Chain[Int]] suffixed", SuffixedTests[Chain[Int], Chain[Int]].suffixed)
  checkAll("SuffixedTests[NonEmptyVector[Int], Vector[Int]] suffixed", SuffixedTests[NonEmptyVector[Int], Vector[Int]].suffixed)
  checkAll("SuffixedTests[NonEmptyList[Int], List[Int]] suffixed", SuffixedTests[NonEmptyList[Int], List[Int]].suffixed)
  checkAll("SuffixedTests[NonEmptyChain[Int], Chain[Int]] suffixed", SuffixedTests[NonEmptyChain[Int], Chain[Int]].suffixed)
  checkAll("SuffixedTests[OneAnd[List, Int], List[Int]] suffixed", SuffixedTests[OneAnd[List, Int], List[Int]].suffixed)
}
