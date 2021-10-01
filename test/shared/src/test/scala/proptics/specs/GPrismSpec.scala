package proptics.specs

import proptics.Prism
import proptics.law.discipline.PrismTests
import proptics.macros.GPrism

class GPrismSpec extends PropticsSuite {
  val genPrism: Prism[Json, JString] = GPrism[Json, JString]

  checkAll("GPrism[Json, JString]", PrismTests(genPrism).prism)
}
