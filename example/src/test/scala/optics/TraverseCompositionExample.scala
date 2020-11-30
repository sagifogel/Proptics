package optics

import scala.util.Try

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.syntax.option._
import cats.syntax.validated._

import proptics.specs.PropticsSuite
import proptics.std.string._
import proptics.unsafe.string._
import proptics.{Traversal, Traversal_}

class TraverseCompositionExample extends PropticsSuite {
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
    def parseInt(str: String): Option[Int] =
      Try(str.toInt).toOption

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
}
