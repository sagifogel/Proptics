package optics

import scala.util.Try

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.instances.char._
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.validated._

import proptics.specs.PropticsSuite
import proptics.std.string._
import proptics.std.tuple._
import proptics.syntax.traversal._
import proptics.unsafe.string._
import proptics.{Traversal, Traversal_}

class TraversalExamples extends PropticsSuite {
  def parseInt(str: String): Option[Int] =
    Try(str.toInt).toOption

  test("travers tuple applies the function to the right element") {
    val fromTraverse = Traversal.fromTraverse[(String, *), String]
    val pair = ("12345", "12345")
    val expected = ("12345", "123")

    assertResult(expected)(fromTraverse.over(_.take(3))(pair))
  }

  test("traverse only the first two elements of a list") {
    val traversal = Traversal.take[List, Int](2)

    assertResult(List(10, 20, 3, 4, 5))(traversal.over(_ * 10)(List.range(1, 6)))
  }

  test("traverse all elements of a list except the two first ones") {
    val traversal = Traversal.drop[List, Int](2)

    assertResult(List(1, 2, 30, 40, 50))(traversal.over(_ * 10)(List.range(1, 6)))
  }

  test("traverse all elements that comes before the first occurrence of 3") {
    val traversal = Traversal.takeWhile[List, Int](_ < 3)

    assertResult(List(10, 20, 3, 4, 5, 2))(traversal.over(_ * 10)(List(1, 2, 3, 4, 5, 2)))
  }

  test("traverse all elements that comes after the first occurrence of 2") {
    val traversal = Traversal.dropWhile[List, Int](_ < 3)

    assertResult(List(1, 2, 30, 40, 50, 20))(traversal.over(_ * 10)(List(1, 2, 3, 4, 5, 2)))
  }

  test("capitalize the title") {
    val traversal = stringToChars compose Traversal.takeWhile[List, Char](_ =!= '-')
    val sentence = "traversal - allows you to traverse over a structure"

    assertResult("TRAVERSAL - allows you to traverse over a structure")(traversal.over(_.toUpper)(sentence))
  }

  test("capitalize the first char of every word") {
    val composed = words compose stringToChars compose Traversal.take[List, Char](1)
    val sentence = "capitalize the first char of every word"

    assertResult("Capitalize The First Char Of Every Word")(composed.over(_.toUpper)(sentence))
  }

  test("find all programming languages with higher kinded types") {
    val hktSupport = Set("Scala", "Haskell")
    val composed =
      Traversal.fromTraverse[List, String] compose
        words compose
        Traversal.filter[String](hktSupport.contains)

    val expected = List("Erlang", "F#", "Scala √", "Haskell √")
    assertResult(expected)(composed.over(_ ++ " √")(List("Erlang", "F#", "Scala", "Haskell")))
  }

  test("parse both elements of the tuple") {
    val tupleBoth: Traversal_[(String, String), (Int, Int), String, Int] =
      Traversal_.both[(*, *), String, Int]

    val both1 = tupleBoth.traverse(("1", "2"))(parseInt)
    val both2 = tupleBoth.traverse(("NaN", "2"))(parseInt)

    assertResult((1, 2).some)(both1)
    assertResult(none[(Int, Int)])(both2)
  }

  test("validate email") {
    def validateEmail(email: String): Validated[List[String], String] =
      if (email.contains("@")) email.valid[List[String]]
      else List(s"missing @: $email").invalid[String]

    val both = Traversal_.both[(*, *), List[String], List[String]]
    val composed = both compose Traversal.fromTraverse[List, String]
    val emails1 = (List("a@b.ai", "b@c.com"), List("c@d.org", "d@e.io"))
    val emails2 = (List("a@b.ai", "b.com"), List("c@d.org", "d.io"))
    val bothRes1: Validated[List[String], (List[String], List[String])] =
      composed.traverse(emails1)(validateEmail)

    val bothRes2: Validated[List[String], (List[String], List[String])] =
      composed.traverse(emails2)(validateEmail)

    assertResult(bothRes1.toList.flatMap { case (l1, l2) => l1 ++ l2 })(List("a@b.ai", "b@c.com", "c@d.org", "d@e.io"))
    assertResult(bothRes2)(Invalid(List("missing @: b.com", "missing @: d.io")))
  }

  test("pull an effect outside the structure") {
    val traversal = Traversal_.fromTraverse[List, Option[Int], Int]
    val listWithSomes = List(1.some, 2.some, 3.some)
    val listWithNones = List(1.some, none[Int], 3.some)

    assertResult(traversal.sequence(listWithSomes))(Some(List(1, 2, 3)))
    assertResult(traversal.sequence(listWithNones))(none[List[Int]])
  }

  test("get a specific element from a composition of traversals") {
    val composed =
      (Traversal.fromTraverse[List, List[Int]] compose
        Traversal.fromTraverse[List, Int])
        .element(6)

    val list = List(List(0, 1, 2), List(3, 4), List(5, 6, 7, 8))
    val expected = List(List(0, 1, 2), List(3, 4), List(5, 600, 7, 8))

    assertResult(6.some)(composed.preview(list))
    assertResult(expected)(composed.over(_ * 100)(list))
  }

  test("replace the second element of a list inside a tuple") {
    val composed = _2[String, List[String]] compose Traversal.element[List, String](1)
    val expected = ("Bond", List("Connery", "Craig", "Moore"))

    assertResult(expected)(composed.set("Craig")(("Bond", List("Connery", "Brosnan", "Moore"))))
  }

  test("capitalize the first two words of in a sentence") {
    val composed = {
      words.take(2) compose
        stringToChars compose
        Traversal.fromTraverse[List, Char]
    }

    val sentence = "capitalize the first two words of in a sentence"
    val expected = "CAPITALIZE THE first two words of in a sentence"

    assertResult(expected)(composed.over(_.toUpper)(sentence))
  }

  test("replace each character of the second word of each sentence in a list") {
    val composed =
      Traversal.fromTraverse[List, String] compose
        words.element(1) compose
        stringToChars compose
        Traversal.fromTraverse[List, Char]

    val list = List("Collapse The Light Into Earth", "Dark Matter", "Heartattack In A Layby")
    val expected = List("Collapse xxx Light Into Earth", "Dark xxxxxx", "Heartattack xx A Layby")

    assertResult(expected)(composed.set('x')(list))
  }

  test("create all possible ways to convert characters to upper case in a word") {
    val composed =
      _1[String, Boolean] compose
        stringToChars compose
        Traversal.fromTraverse[List, Char]

    val expected = List(("ABC", true), ("ABc", true), ("AbC", true), ("Abc", true), ("aBC", true), ("aBc", true), ("abC", true), ("abc", true))
    assertResult(expected)(composed.traverse[List](("abc", true))(c => List(c.toUpper, c.toLower)))
  }
}
