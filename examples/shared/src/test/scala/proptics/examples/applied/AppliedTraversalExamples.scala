package proptics.examples.applied

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.implicits.none
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.semigroup._

import proptics.examples.{parseInt, validateEmail}
import proptics.instances.each._
import proptics.instances.reverse._
import proptics.specs.PropticsSuite
import proptics.std.string._
import proptics.syntax.all._

class AppliedTraversalExamples extends PropticsSuite {
  test("listTraversal of a tuple applies the function to the right element") {
    val expected = ("12345", "123")
    val res =
      ("12345", "12345").traversal
        .over(_.take(3))

    assertResult(expected)(res)
  }

  test("Each of a tuple applies the function focus both elements") {
    val expected = ("123", "123")
    val res =
      ("12345", "12345").each
        .over(_.take(3))

    assertResult(expected)(res)
  }

  test("listTraversal only the first two elements of a list") {
    val expected = List(10, 20, 3, 4, 5)
    val list = List(1, 2, 3, 4, 5)
    val traversal = list.traversal.take(2)
    val each = list.each.take(2)
    val traversalResult = traversal.over(_ * 10)
    val eachResult = each.over(_ * 10)

    assertResult(expected)(traversalResult)
    assertResult(traversalResult)(eachResult)
  }

  test("listTraversal all elements of a list except the two first ones") {
    val expected = List(1, 2, 30, 40, 50)
    val list = List(1, 2, 3, 4, 5)
    val traversal = list.traversal.drop(2)
    val each = list.each.drop(2)
    val traversalResult = traversal.over(_ * 10)
    val eachResult = each.over(_ * 10)

    assertResult(expected)(traversalResult)
    assertResult(traversalResult)(eachResult)
  }

  test("listTraversal all elements that come before the first occurrence of 3") {
    val expected = List(10, 20, 3, 4, 5, 2)
    val list = List(1, 2, 3, 4, 5, 2)
    val traversal = list.traversal.takeWhile(_ < 3)
    val each = list.each.takeWhile(_ < 3)
    val traversalResult = traversal.over(_ * 10)
    val eachResult = each.over(_ * 10)

    assertResult(expected)(traversalResult)
    assertResult(traversalResult)(eachResult)
  }

  test("listTraversal all elements starting from the first occurrence of 3") {
    val expected = List(1, 2, 30, 40, 50, 20)
    val list = List(1, 2, 3, 4, 5, 2)
    val traversal = list.each.dropWhile(_ < 3)
    val each = list.each.dropWhile(_ < 3)
    val traversalResult = traversal.over(_ * 10)
    val eachResult = each.over(_ * 10)

    assertResult(expected)(traversalResult)
    assertResult(traversalResult)(eachResult)
  }

  test("capitalize the title") {
    val expected = "TRAVERSAL - allows you to listTraversal over a structure"
    val sentence = "traversal - allows you to listTraversal over a structure"
    val traversal = sentence.toChars.takeWhile(_ =!= '-')

    assertResult(expected)(traversal.over(_.toUpper))
  }

  test("capitalize the first char of every word") {
    val expected = "Capitalize The First Char Of Every Word"
    val sentence = "capitalize the first char of every word"
    val traversal = sentence.toWords.toChars.take(1)

    assertResult(expected)(traversal.over(_.toUpper))
  }

  test("find all programming languages with higher kinded types") {
    val hktSupport = Set("Scala", "Haskell")
    val expected = List("Erlang", "F#", "Scala √", "Haskell √")
    val languages = List("Erlang", "F#", "Scala", "Haskell")
    val traversal = languages.traversal.toWords.filter(hktSupport.contains)
    val each = languages.each.toWords.filter(hktSupport.contains)
    val traversalResult = traversal.over(_ |+| " √")
    val eachResult = each.over(_ |+| " √")

    assertResult(expected)(traversalResult)
    assertResult(traversalResult)(eachResult)
  }

  test("parse both elements of the tuple") {
    val both1 = ("1", "2").bitraverse_[Int].traverse(parseInt)
    val both2 = ("NaN", "2").bitraverse_[Int].traverse(parseInt)

    assertResult((1, 2).some)(both1)
    assertResult(none[(Int, Int)])(both2)
  }

  test("validate email") {
    val bothRes1 =
      (List("a@b.ai", "b@c.com"), List("c@d.org", "d@e.io"))
        .bitraverse_[List[String]]
        .andThenTraverse
        .traverse(validateEmail)

    val bothRes2: Validated[List[String], (List[String], List[String])] =
      (List("a@b.ai", "b.com"), List("c@d.org", "d.io"))
        .bitraverse_[List[String]]
        .andThenTraverse
        .traverse(validateEmail)

    assertResult(bothRes1.toList.flatMap { case (l1, l2) => l1 ++ l2 })(List("a@b.ai", "b@c.com", "c@d.org", "d@e.io"))
    assertResult(bothRes2)(Invalid(List("missing @: b.com", "missing @: d.io")))
  }

  test("pull an effect outside the structure") {
    val sequenced1 = List(1.some, 2.some, 3.some).traversal_[Int].sequence
    val sequenced2 = List(1.some, none[Int], 3.some).traversal_[Int].sequence

    assertResult(Some(List(1, 2, 3)))(sequenced1)
    assertResult(none[List[Int]])(sequenced2)
  }

  test("get a specific element from a composition of traversals") {
    val list = List(List(0, 1, 2), List(3, 4), List(5, 6, 7, 8))
    val expected = List(List(0, 1, 2), List(3, 4), List(5, 600, 7, 8))
    val composedTraversal = list.traversal.andThenTraverse.single(6)
    val composedEach = list.each.andThenTraverse.single(6)
    val traversalResult = composedTraversal.over(_ * 100)
    val eachResult = composedEach.over(_ * 100)

    assertResult(expected)(traversalResult)
    assertResult(traversalResult)(eachResult)
  }

  test("capitalize the first two words of in a sentence") {
    val sentence = "capitalize the first two words of in a sentence"
    val traversal =
      sentence.toWords
        .take(2)
        .toChars
        .andThenTraverse

    val expected = "CAPITALIZE THE first two words of in a sentence"

    assertResult(expected)(traversal.over(_.toUpper))
  }

  test("replace each character of the second word of each sentence in a list") {
    val list = List("Collapse The Light Into Earth", "Dark Matter", "Heartattack In A Layby")
    val expected = List("Collapse xxx Light Into Earth", "Dark xxxxxx", "Heartattack xx A Layby")
    val traversal =
      list.traversal
        .andThen(words.single(1))
        .andThen(stringToChars)
        .andThenTraverse
    val each =
      list.each
        .andThen(words.single(1))
        .andThen(stringToChars)
        .andThenTraverse
    val traversalResult = traversal.set('x')
    val eachResult = each.set('x')

    assertResult(expected)(traversalResult)
    assertResult(traversalResult)(eachResult)
  }

  test("view all elements of a List in a reversed order") {
    val reversed = (2, List(1, 2, 3, 4)).traversal.reverse
    assertResult(List(4, 3, 2, 1))(reversed.view)
  }
}
