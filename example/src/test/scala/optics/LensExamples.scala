package optics

import proptics.specs.PropticsSuite
import proptics.std.tuple._
import proptics.syntax.lens._

class LensExamples extends PropticsSuite {
  test("pull effect outside the structure") {
    val polymorphicFst = _1P[Option[String], String, String]
    val inputSome: (Option[String], String) = (Some("Of These Days"), "One Of These Days")
    val inputNone: (Option[String], String) = (None, "Of These Days")
    val expected = Some(("Of These Days", "One Of These Days"))

    assertResult(expected)(polymorphicFst.sequence(inputSome))
    assertResult(None)(polymorphicFst.sequence(inputNone))
  }
}
