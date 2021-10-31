package proptics.examples.applied

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.implicits.none
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.semigroup._

import proptics.examples.{parseInt, validateEmail}
import proptics.instances.each._
import proptics.specs.PropticsSuite
import proptics.std.string._
import proptics.syntax.all._

class AppliedTraversalExamples extends PropticsSuite {
  test("listTraversal of a tuple applies the function to the right element") {
    val expected = ("12345", "123")
    val res =
      ("12345", "12345").fromTraverse
        .over(_.take(3))

    assertResult(expected)(res)
  }

  test("listTraversal only the first two elements of a list") {
    val expected = List(10, 20, 3, 4, 5)
    val traversal = List(1, 2, 3, 4, 5).each.take(2)

    assertResult(expected)(traversal.over(_ * 10))
  }

  test("listTraversal all elements of a list except the two first ones") {
    val expected = List(1, 2, 30, 40, 50)
    val traversal = List(1, 2, 3, 4, 5).each.drop(2)

    assertResult(expected)(traversal.over(_ * 10))
  }

  test("listTraversal all elements that come before the first occurrence of 3") {
    val expected = List(10, 20, 3, 4, 5, 2)
    val traversal = List(1, 2, 3, 4, 5, 2).each.takeWhile(_ < 3)

    assertResult(expected)(traversal.over(_ * 10))
  }

  test("listTraversal all elements starting from the first occurrence of 3") {
    val expected = List(1, 2, 30, 40, 50, 20)
    val traversal = List(1, 2, 3, 4, 5, 2).each.dropWhile(_ < 3)

    assertResult(expected)(traversal.over(_ * 10))
  }

  test("capitalize the title") {
    val expected = "TRAVERSAL - allows you to listTraversal over a structure"
    val sentence = "traversal - allows you to listTraversal over a structure"
    val traversal = sentence.each(stringToChars).takeWhile(_ =!= '-')

    assertResult(expected)(traversal.over(_.toUpper))
  }

  test("capitalize the first char of every word") {
    val expected = "Capitalize The First Char Of Every Word"
    val sentence = "capitalize the first char of every word"
    val traversal = sentence.each(words).andThen(stringToChars).take(1)

    assertResult(expected)(traversal.over(_.toUpper))
  }

  test("find all programming languages with higher kinded types") {
    val hktSupport = Set("Scala", "Haskell")
    val expected = List("Erlang", "F#", "Scala √", "Haskell √")
    val traversal =
      List("Erlang", "F#", "Scala", "Haskell").fromTraverse
        .andThen(words)
        .filter(hktSupport.contains)

    assertResult(expected)(traversal.over(_ |+| " √"))
  }

  test("parse both elements of the tuple") {
    val both1 = ("1", "2").both_[Int].traverse(parseInt)
    val both2 = ("NaN", "2").both_[Int].traverse(parseInt)

    assertResult((1, 2).some)(both1)
    assertResult(none[(Int, Int)])(both2)
  }

  test("validate email") {
    val bothRes1 =
      (List("a@b.ai", "b@c.com"), List("c@d.org", "d@e.io"))
        .both_[List[String]]
        .andThenT
        .traverse(validateEmail)

    val bothRes2: Validated[List[String], (List[String], List[String])] =
      (List("a@b.ai", "b.com"), List("c@d.org", "d.io"))
        .both_[List[String]]
        .andThenT
        .traverse(validateEmail)

    assertResult(bothRes1.toList.flatMap { case (l1, l2) => l1 ++ l2 })(List("a@b.ai", "b@c.com", "c@d.org", "d@e.io"))
    assertResult(bothRes2)(Invalid(List("missing @: b.com", "missing @: d.io")))
  }

  test("pull an effect outside the structure") {
    val sequenced1 = List(1.some, 2.some, 3.some).fromTraverse_[Int].sequence
    val sequenced2 = List(1.some, none[Int], 3.some).fromTraverse_[Int].sequence

    assertResult(Some(List(1, 2, 3)))(sequenced1)
    assertResult(none[List[Int]])(sequenced2)
  }

  test("get a specific element from a composition of traversals") {
    val composed =
      List(List(0, 1, 2), List(3, 4), List(5, 6, 7, 8)).fromTraverse.andThenT
        .elementAt(6)
    val expected = List(List(0, 1, 2), List(3, 4), List(5, 600, 7, 8))

    assertResult(6.some)(composed.preview)
    assertResult(expected)(composed.over(_ * 100))
  }

  test("capitalize the first two words of in a sentence") {
    val sentence = "capitalize the first two words of in a sentence"
    val traversal =
      sentence
        .each(words)
        .take(2)
        .andThen(stringToChars)
        .andThenT

    val expected = "CAPITALIZE THE first two words of in a sentence"

    assertResult(expected)(traversal.over(_.toUpper))
  }

  test("replace each character of the second word of each sentence in a list") {
    val list = List("Collapse The Light Into Earth", "Dark Matter", "Heartattack In A Layby")
    val expected = List("Collapse xxx Light Into Earth", "Dark xxxxxx", "Heartattack xx A Layby")
    val traversal =
      list.fromTraverse
        .andThen(words.elementAt(1))
        .andThen(stringToChars)
        .andThenT

    assertResult(expected)(traversal.set('x'))
  }
}
