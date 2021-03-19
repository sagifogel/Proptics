package optics

import cats.instances.either._
import cats.syntax.either._

import proptics.Iso
import proptics.Iso._
import proptics.instances.field2._
import proptics.instances.reverse._
import proptics.specs.PropticsSuite
import proptics.std.either._
import proptics.std.list._
import proptics.std.string._
import proptics.std.tuple._
import proptics.syntax.iso._

class IsoExamples extends PropticsSuite {
  test("swap Tuple") {
    val tuple = ("Hello", 9)

    assertResult(tuple.swap)(swapTuple[String, Int].view(tuple))
  }

  test("swap Either") {
    val either: Either[String, Int] = "Hello".asLeft[Int]

    assertResult(either.swap)(swapEither[String, Int].view(either))
  }

  test("swap Either twice") {
    val either: Either[String, Int] = "Hello".asLeft[Int]
    val swapTwice: Iso[Either[String, Int], Either[String, Int]] =
      swapEither compose swapEither

    assertResult(either)(swapTwice.view(either))
  }

  test("replace the case of all characters using involuted") {
    val composed =
      _2[Int, String] compose
        stringToChars compose
        involuted[List[Char]](_.map(c => if (c.isUpper) c.toLower else c.toUpper)) compose
        charsToString

    val input = (9, "Hi")
    assertResult((9, "camelCase"))(composed.set("CAMELcASE")(input))
  }

  test("reverse the string of an either using map") {
    val composed =
      stringToChars compose
        reverse[List[Char], List[Char]] compose
        charsToString

    val input = Right("desrever")
    assertResult(Right("reversed"))(composed.map[Either[Int, *]] view input)
    assertResult(Left(9))(composed.map[Either[Int, *]] view Left(9))
  }

  test("reverse both sides of an either using bimap") {
    val composed =
      stringToChars compose
        reverse[List[Char], List[Char]] compose
        charsToString

    assertResult(Right("reversed"))(composed.bimap[Either] view Right("desrever"))
    assertResult(Left("reversed"))(composed.bimap[Either] view Left("desrever"))
  }

  test("using contramap to create a string from boolean") {
    val negate = Iso.involuted[Boolean](!_).contramap[* => String]

    assertResult("false")(negate.view(_.toString)(true))
  }
}
