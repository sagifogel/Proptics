package proptics.specs

import proptics.instances.functorWithIndex._
import proptics.law.discipline.FunctorWithIndexTests

class FunctorWithIndexSpec extends FunctorWithIndexSpec0 {
  checkAll("FunctorWithIndex[Stream, Int]", FunctorWithIndexTests[Stream, Int].functorWithIndex[Int, Int, Int])
}
