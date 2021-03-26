package proptics.specs

import scala.collection.immutable.ArraySeq

import proptics.instances.suffixed._
import proptics.law.discipline.SuffixedTests

class SuffixedSpec extends SuffixedSpec0 {
  checkAll("SuffixedTests[LazyList[Int], LazyList[Int]] suffixed", SuffixedTests[LazyList[Int], LazyList[Int]].suffixed)
  checkAll("SuffixedTests[ArraySeq[Int], ArraySeq[Int]] suffixed", SuffixedTests[ArraySeq[Int], ArraySeq[Int]].suffixed)
}
