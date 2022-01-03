package proptics.examples.applied

import cats.syntax.option._

import proptics.examples.{Address, Person, Street, mrWhite}
import proptics.instances.all._
import proptics.specs.PropticsSuite
import proptics.syntax.all._

class AppliedLensExamples extends PropticsSuite {
  test("pull an effect outside the structure") {
    val inputSome = (Option("Of These Days"), "One Of These Days").first_[String]
    val inputNon = (None: Option[String], "Of These Days").first_[String]
    val expected: Option[(String, String)] = Some(("Of These Days", "One Of These Days"))

    assertResult(expected)(inputSome.sequence)
    assertResult(None)(inputNon.sequence)
  }

  test("apply the first Lens three times in order to reach the leftmost element") {
    val leftmost = ((("Hi!", 3), 2), 1).first.first.first

    assertResult("Hi!")(leftmost.view)
    assertResult(((("Bye!", 3), 2), 1))(leftmost.set("Bye!"))
  }

  test("deeply nested record using lens") {
    val person = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 9)))
    val streetNumberLens = person.lens(_.address.street.number)

    assertResult(mrWhite)(streetNumberLens.set(308))
  }

  test("view all elements of a List in a reversed order") {
    val reversed = (2, List(1, 2, 3, 4)).second.reverse
    assertResult(List(4, 3, 2, 1))(reversed.view)
  }

  test("using at in order to change an element in a Map") {
    val lensMap = (2, Map("a" -> "1", "b" -> "2"), 4).second.at("a")
    val expected = (2, Map("a" -> "9", "b" -> "2"), 4)

    assertResult(expected)(lensMap.set("9".some))
  }

  test("using at in order to remove an element from a Map") {
    val lensMap = (2, Map("a" -> "1", "b" -> "2"), 4).second.at("a")
    val expected = (2, Map("b" -> "2"), 4)

    assertResult(expected)(lensMap.set(None))
  }
}
