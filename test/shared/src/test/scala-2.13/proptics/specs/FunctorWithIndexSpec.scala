package proptics.specs

import scala.collection.compat.immutable.ArraySeq

import proptics.instances.functorWithIndex._
import proptics.law.discipline.FunctorWithIndexTests

class FunctorWithIndexSpec extends FunctorWithIndexSpec0 {
  checkAll("FunctorWithIndex[LazyList, Int]", FunctorWithIndexTests[LazyList, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndex[ArraySeq, Int]", FunctorWithIndexTests[ArraySeq, Int].functorWithIndex[Int, Int, Int])
}
