package proptics.specs

import scala.collection.compat.immutable.ArraySeq

import proptics.instances.reverse._
import proptics.law.discipline.ReverseTests

class ReverseSpec extends ReverseSpec0 {
  checkAll("ReverseTests[LazyList[Int], LazyList[Int]] reverse", ReverseTests[LazyList[Int], LazyList[Int]].reverse)
  checkAll("ReverseTests[ArraySeq[Int], ArraySeq[Int]] reverse", ReverseTests[ArraySeq[Int], ArraySeq[Int]].reverse)
}
