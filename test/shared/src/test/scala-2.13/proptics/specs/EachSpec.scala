package proptics.specs

import scala.collection.immutable.ArraySeq

import proptics.instances.each._
import proptics.law.discipline.EachTests

class EachSpec extends EachSpec0 {
  checkAll("Each[LazyList[Int], Int] Each", EachTests[LazyList[Int], Int].each)
  checkAll("Each[ArraySeq[Int], Int] Each", EachTests[ArraySeq[Int], Int].each)
}
