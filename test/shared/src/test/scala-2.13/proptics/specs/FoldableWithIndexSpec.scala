package proptics.specs

import scala.collection.compat.immutable.ArraySeq

import proptics.instances.foldableWithIndex._
import proptics.law.discipline.FoldableWithIndexTests

class FoldableWithIndexSpec extends FoldableWithIndexSpec0 {
  checkAll("FoldableWithIndex[LazyList, Int]", FoldableWithIndexTests[LazyList, Int].foldableWithIndex[Int, Int, Int])
  checkAll("FoldableWithIndex[ArraySeq, Int]", FoldableWithIndexTests[ArraySeq, Int].foldableWithIndex[Int, Int, Int])
}
