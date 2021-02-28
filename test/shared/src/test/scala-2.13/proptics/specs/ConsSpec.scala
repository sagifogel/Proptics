package proptics.specs

import scala.collection.compat.immutable.ArraySeq

import proptics.instances.cons._
import proptics.law.discipline.ConsTests

class ConsSpec extends ConsSpec0 {
  checkAll("Cons[LazyList[Int]]", ConsTests[LazyList[Int], Int].cons)
  checkAll("Cons[ArraySeq[Int]]", ConsTests[ArraySeq[Int], Int].cons)
}
