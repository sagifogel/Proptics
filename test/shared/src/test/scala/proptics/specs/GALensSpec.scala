package proptics.specs

import proptics.ALens
import proptics.law.discipline.ALensTests
import proptics.macros.GALens

class GALensSpec extends PropticsSuite {
  val firstLevelGALens: ALens[Person, String] = GALens[Person](_.name)
  val leafLevelGALens: ALens[Person, Int] = GALens[Person](_.address.street.number)

  checkAll("GALens[Person, String] top level gen", ALensTests(firstLevelGALens).aLens)
  checkAll("GALens[Person, Int] leaf level gen", ALensTests(leafLevelGALens).aLens)
}
