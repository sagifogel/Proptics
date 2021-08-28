package optics.examples

import optics._
import proptics.instances.field1._
import proptics.instances.field2._
import proptics.specs.PropticsSuite
import proptics.std.tuple._
import proptics.syntax.function._
import proptics.syntax.lens._
import proptics.{Lens, Lens2}

class LensExamples extends PropticsSuite {
  val address: Lens[Person, Address] =
    Lens[Person, Address](_.address)(person => address => person.copy(address = address))

  val street: Lens[Address, Street] =
    Lens[Address, Street](_.street)(address => street => address.copy(street = street))

  val streetNumber: Lens[Street, Int] =
    Lens[Street, Int](_.number)(street => number => street.copy(number = number))

  val personStreet: Lens[Person, Int] = address andThen street andThen streetNumber

  test("pull an effect outside the structure") {
    val polymorphicFst = _1P[Option[String], String, String]
    val inputSome: (Option[String], String) = (Some("Of These Days"), "One Of These Days")
    val inputNone: (Option[String], String) = (None, "Of These Days")
    val expected: Option[(String, String)] = Some(("Of These Days", "One Of These Days"))

    assertResult(expected)(polymorphicFst.sequence(inputSome))
    assertResult(None)(polymorphicFst.sequence(inputNone))
  }

  test("apply the _1 Lens three times in order to reach the leftmost element") {
    val leftmost =
      _1[((String, Int), Int), Int] andThen
        _1[(String, Int), Int] andThen
        _1[String, Int]

    val tupled: (((String, Int), Int), Int) = ((("Hi!", 3), 2), 1)
    val expected = ((("Bye!", 3), 2), 1)

    assertResult("Hi!")(leftmost.view(tupled))
    assertResult(expected)(leftmost.set("Bye!")(tupled))
  }

  test("focus on two distinct parts of a structure") {
    val lens = Lens2[UserRegistration, String, Int](_.userName, _.yearOfBirth) { (reg, name, year) =>
      reg.copy(userName = name, yearOfBirth = year)
    }

    assertResult(("User99", 2000))(lens.view(UserRegistration("User99", "Password!", 2000)))
  }

  test("deeply nested record using compose") {
    val composed = streetNumber compose street compose address
    val person = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 9)))

    assertResult(mrWhite)(composed.set(308)(person))
  }

  test("deeply nested record using andThen") {
    val composed = address andThen street andThen streetNumber
    val person = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 9)))

    assertResult(mrWhite)(composed.set(308)(person))
  }

  test("using lenses in order to extract nested data within data structures") {
    val composed =
      _2[String, (List[Int], String)] andThen
        _1[List[Int], String]

    val list = List(("A", (List(1, 2, 3), "targets")), ("B", (List(4, 5), "targets")))
    assertResult(List(1, 2, 3, 4, 5))(list.flatMap(composed view))
  }

  test("using apply flipped syntax") {
    val personNameLens: Lens[Person, String] =
      Lens[Person, String](_.name)(person => name => person.copy(name = name))

    val person = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 9)))
    val expected = Person("Skyler White", Address("Albuquerque", Street("Negra Arroyo Lane", 308)))
    val composed = address andThen street andThen streetNumber
    val res = person &
      personNameLens.over(_.replace("Walter", "Skyler")) &
      composed.set(308)

    assertResult(expected)(res)
  }
}
