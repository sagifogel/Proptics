package optics

import scala.util.Try

import cats.instances.option._
import cats.syntax.option._

import proptics.specs.PropticsSuite
import proptics.std.string._
import proptics.{Fold, Fold_, Lens}

final case class Language(name: String, designer: String)

class FoldExamples extends PropticsSuite {
  def parseInt(str: String): Option[Int] =
    Try(str.toInt).toOption

  test("fold over an option") {
    val fold = Fold.fromFoldable[Option, String]

    assertResult(List("value"))(fold.viewAll(Some("value")))
  }

  test("fold over a tuple") {
    val fold = Fold.fromFoldable[(Int, *), String]

    assertResult(List("value"))(fold.viewAll((9, "value")))
  }

  test("using lenses as folds") {
    val languages = List(
      Language("Haskell", "Simon Peyton Jones"),
      Language("Clojure", "Rich Hickey"),
      Language("Scala", "Martin Oderski"),
      Language("Erlang", "Joe Armstrong"),
      Language("Haskell", "Philip Wadler")
    )

    // implicit cast from [[Lens_]] to [[Fold_]]
    val designer: Fold[Language, String] =
      Lens[Language, String](_.designer)(lang => designer => lang.copy(designer = designer))
    val composed = Fold.fromFoldable[List, Language] compose designer

    assertResult(languages.map(_.designer))(composed.viewAll(languages))
  }

  test("compose folds") {
    val composed =
      Fold.fromFoldable[List, (Int, String)] compose
        Fold.fromFoldable[(Int, *), String] compose
        stringToChars compose
        Fold.fromFoldable[List, Char]

    val input = List((0, "Govan"), (1, "Abassi"), (2, "Gambale"))
    assertResult("GovanAbassiGambale")(composed.viewAll(input).mkString)
  }

  test("parse both elements of the tuple") {
    val tupleBoth: Fold_[(String, String), (Int, Int), String, Int] =
      Fold_.both[(*, *), String, Int]

    val both1 = tupleBoth.foldMap(("1", "2"))(parseInt)
    val both2 = tupleBoth.foldMap(("NaN", "2"))(parseInt)

    assertResult(3.some)(both1)
    assertResult(2.some)(both2)
  }
}
