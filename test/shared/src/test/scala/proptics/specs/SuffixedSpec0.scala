package proptics.specs

import cats.data._

import proptics.instances.suffixed._
import proptics.law.discipline.SuffixedTests

private[specs] trait SuffixedSpec0 extends PropticsSuite {
  checkAll("SuffixedTests[String, String] suffix", SuffixedTests[String, String].suffixed)
  checkAll("SuffixedTests[Array[Int], Array[Int]] suffix", SuffixedTests[Array[Int], Array[Int]].suffixed)
  checkAll("SuffixedTests[Vector[Int], Vector[Int]] suffix", SuffixedTests[Vector[Int], Vector[Int]].suffixed)
  checkAll("SuffixedTests[List[Int], List[Int]] suffix", SuffixedTests[List[Int], List[Int]].suffixed)
  checkAll("SuffixedTests[Chain[Int], Chain[Int]] suffix", SuffixedTests[Chain[Int], Chain[Int]].suffixed)
  checkAll("SuffixedTests[NonEmptyVector[Int], Vector[Int]] suffix", SuffixedTests[NonEmptyVector[Int], Vector[Int]].suffixed)
  checkAll("SuffixedTests[NonEmptyList[Int], List[Int]] suffix", SuffixedTests[NonEmptyList[Int], List[Int]].suffixed)
  checkAll("SuffixedTests[NonEmptyChain[Int], Chain[Int]] suffix", SuffixedTests[NonEmptyChain[Int], Chain[Int]].suffixed)
  checkAll("SuffixedTests[OneAnd[List, Int], List[Int]] suffix", SuffixedTests[OneAnd[List, Int], List[Int]].suffixed)
}
