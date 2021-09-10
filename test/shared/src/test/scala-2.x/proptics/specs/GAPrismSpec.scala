package proptics.specs

import proptics.APrism
import proptics.law.discipline.APrismTests
import proptics.macros.GAPrism

class GAPrismSpec extends PropticsSuite {
  val genAPrism: APrism[Json, JString] = GAPrism[Json, JString]

  checkAll("GAPrism[Json, JString]", APrismTests(genAPrism).aPrism)
}
