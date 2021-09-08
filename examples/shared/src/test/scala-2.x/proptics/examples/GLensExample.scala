package proptics.examples

import proptics.Lens
import proptics.macros.GLens
import proptics.specs.PropticsSuite

class GLensExample extends PropticsSuite {
  test("using GLens to generate Lens") {
    val personNameLens: Lens[Person, String] = GLens[Person](_.name)
    val person = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 308)))
    val expected = Person("Skyler White", Address("Albuquerque", Street("Negra Arroyo Lane", 308)))
    val res = personNameLens.over(_.replace("Walter", "Skyler"))(person)

    assertResult(expected)(res)
  }

  test("using GLens to generate composite Lens") {
    val personNameLens: Lens[Person, Int] = GLens[Person](_.address.street.number)
    val person = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 9)))
    val expected = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 308)))
    val res = personNameLens.set(308)(person)

    assertResult(expected)(res)
  }
}
