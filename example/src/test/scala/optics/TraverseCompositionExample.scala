package optics

import scala.util.Try

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.instances.char._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.validated._

import proptics.specs.PropticsSuite
import proptics.std.all._2
import proptics.std.string._
import proptics.syntax.traversal._
import proptics.unsafe.string._
import proptics.{Traversal, Traversal_}

class TraverseCompositionExample extends PropticsSuite {
  def parseInt(str: String): Option[Int] =
    Try(str.toInt).toOption

  test("travers tuple applies the function to the right element") {
    val fromTraverse = Traversal.fromTraverse[(String, *), String]
    val pair = ("12345", "12345")
    val expected = ("12345", "123")

    assertResult(fromTraverse.over(_.take(3))(pair))(expected)
  }

  test("traverse only the first two elements of a list") {
    val traversal = Traversal.take[List, Int](2)

    assertResult(traversal.over(_ * 10)(List.range(1, 6)))(List(10, 20, 3, 4, 5))
  }

  test("traverse all elements of a list except the two first ones") {
    val traversal = Traversal.drop[List, Int](2)

    assertResult(traversal.over(_ * 10)(List.range(1, 6)))(List(1, 2, 30, 40, 50))
  }

  test("traverse all elements that comes before the first occurrence of 3") {
    val traversal = Traversal.takeWhile[List, Int](_ < 3)

    assertResult(traversal.over(_ * 10)(List(1, 2, 3, 4, 5, 2)))(List(10, 20, 3, 4, 5, 2))
  }

  test("traverse all elements that comes after the first occurrence of 2") {
    val traversal = Traversal.dropWhile[List, Int](_ < 3)

    assertResult(traversal.over(_ * 10)(List(1, 2, 3, 4, 5, 2)))(List(1, 2, 30, 40, 50, 20))
  }

  test("capitalize the title") {
    val traversal = stringToChars compose Traversal.takeWhile[List, Char](_ =!= '-')
    val sentence = "traversal - allows you to traverse over a structure"

    assertResult(traversal.over(_.toUpper)(sentence))("TRAVERSAL - allows you to traverse over a structure")
  }

  test("capitalize the first char of every word") {
    val composed = words compose stringToChars compose Traversal.take[List, Char](1)
    val sentence = "capitalize the first char of every word"

    assertResult(composed.over(_.toUpper)(sentence))("Capitalize The First Char Of Every Word")
  }

  test("find all programming languages with higher kinded types") {
    val hktSupport = Set("Scala", "Haskell")
    val composed: Traversal_[List[String], List[String], String, String] =
      Traversal.fromTraverse[List, String] compose
        words compose
        Traversal.filter[String](hktSupport.contains)

    val expected = List("Erlang", "F#", "Scala √", "Haskell √")
    assertResult(composed.over(_ ++ " √")(List("Erlang", "F#", "Scala", "Haskell")))(expected)
  }

  test("parse both elements of the tuple") {
    val tupleBoth: Traversal_[(String, String), (Int, Int), String, Int] =
      Traversal_.both[(*, *), String, Int]

    val both1 = tupleBoth.traverse(("1", "2"))(parseInt)
    val both2 = tupleBoth.traverse(("NaN", "2"))(parseInt)

    assertResult(both1)((1, 2).some)
    assertResult(both2)(none[(Int, Int)])
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

  test("sequence") {
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

    assertResult(composed.preview(list))(6.some)
    assertResult(composed.over(_ * 100)(list))(expected)
  }

  test("replace the second element of a list inside a tuple") {
    val composed = _2[String, List[String]] compose Traversal.element[List, String](1)
    val res = composed.set("Craig")(("Bond", List("Connery", "Brosnan", "Moore")))

    assertResult(res)(("Bond", List("Connery", "Craig", "Moore")))
  }

}
