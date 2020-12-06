package optics

import proptics.specs.PropticsSuite
import proptics.std.tuple._
import proptics.syntax.lens._
import proptics.unsafe.Lens

final case class UserRegistration(userName: String, password: String, yearOfBirth: Int)

class LensExamples extends PropticsSuite {
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
      _1[((String, Int), Int), Int] compose
        _1[(String, Int), Int] compose
        _1[String, Int]

    val tupled: (((String, Int), Int), Int) = ((("Hi!", 3), 2), 1)
    val expected = ((("Bye!", 3), 2), 1)

    assertResult("Hi!")(leftmost.view(tupled))
    assertResult(expected)(leftmost.set("Bye!")(tupled))
  }

  test("focus on two distinct parts of a structure") {
    val lens = Lens.apply2[UserRegistration, String, Int](_.userName, _.yearOfBirth) { (reg, name, year) =>
      reg.copy(userName = name, yearOfBirth = year)
    }

    assertResult(("User99", 2000))(lens.view(UserRegistration("User99", "Password!", 2000)))
  }
}
