package proptics.specs

import scala.collection.compat.immutable.ArraySeq

import proptics.instances.empty._
import proptics.law.discipline.EmptyTests

class EmptySpec extends EmptySpec0 {
  checkAll("Empty[LazyList[Int]]", EmptyTests[LazyList[Int]].empty)
  checkAll("Empty[ArraySeq[Int]]", EmptyTests[ArraySeq[Int]].empty)
}
