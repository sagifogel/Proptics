package proptics.specs

import proptics.instances.foldableWithIndex._
import proptics.law.discipline.FoldableWithIndexTests

class FoldableWithIndexSpec extends FoldableWithIndexSpec0 {
  checkAll("FoldableWithIndex[Stream, Int]", FoldableWithIndexTests[Stream, Int].foldableWithIndex[Int, Int, Int])
}
