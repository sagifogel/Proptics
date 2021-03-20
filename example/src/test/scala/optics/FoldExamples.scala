package optics

import scala.util.Try

import cats.instances.map._
import cats.instances.option._
import cats.kernel.{Eq, Order}
import cats.syntax.option._
import spire.std.boolean._
import spire.std.int._

import proptics.specs.PropticsSuite
import proptics.std.string._
import proptics.syntax.traversal._
import proptics.unsafe.std.string._
import proptics.{Fold, Getter, _}

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
    val composed: Fold[List[Language], String] = Fold.fromFoldable[List, Language] compose designer

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

  test("sum all number of episodes") {
    val numberOfEpisodes =
      Fold.fromFoldable[List, TVShow] compose
        Fold[TVShow, Int](_.numEpisodes)

    assertResult(125)(numberOfEpisodes.sum(tvShows))
  }

  test("get the maximum score of all tv shows") {
    val maxScore =
      Fold.fromFoldable[List, TVShow] compose
        Fold[TVShow, Int](_.criticScore)

    assertResult(97.some)(maxScore.maximum(tvShows))
  }

  test("get the name of tv show that has the highest score") {
    implicit val tvShowOrder: Order[TVShow] = Order.by(_.criticScore)
    val fold = Fold.fromFoldable[List, TVShow]

    assertResult("Better Call Saul".some)(fold.maximum(tvShows).map(_.title))
  }

  test("get the name of the oldest actor") {
    implicit val actorsOrder: Order[Actor] = Order.by(_.birthYear)
    val actors = Fold.fromFoldable[List, TVShow] compose
      Fold[TVShow, List[Actor]](_.actors) compose
      Fold.fromFoldable[List, Actor]

    assertResult("Jonathan Banks".some)(actors.minimum(tvShows).map(_.name))
  }

  test("actors who participate in more than one show") {
    val actors = Fold.fromFoldable[List, TVShow] compose
      Fold[TVShow, Map[String, Int]](_.actors.map(_.name -> 1).toMap)

    val expected = List("Jonathan Banks", "Giancarlo Esposito", "Bob Odenkirk") toSet
    val actorsInBothShows =
      actors.fold(tvShows).collect { case (actor, numOShows) if numOShows > 1 => actor } toSet

    assertResult(expected)(actorsInBothShows)
  }

  test("View all actors that their first name starts with the letter 'A'") {
    val fold =
      Fold.fromFoldable[List, TVShow] to (_.actors) compose
        Fold.fromFoldable[List, Actor] to [String, String] (_.name) compose
        Fold.filter[String](_.startsWith("A")) compose
        words.take(1)

    assertResult(List("Aaron", "Anna"))(fold.viewAll(tvShows))
  }

  test("using fold as a predicate, count the number of awards of all actors that were nominated for golden globe but did not win") {
    implicit val eqActor: Eq[Award] = Eq.fromUniversalEquals[Award]
    val foldPredicate = Getter[Actor, Award](_.nomiation) compose Prism.only[Award](GoldenGlobe)
    val fold =
      Getter[TVShow, List[Actor]](_.actors) compose
        Fold.fromFoldable[List, Actor] compose
        Fold.filter(foldPredicate) compose
        Getter[Actor, List[Award]](_.awards) compose
        Fold.filter[List[Award]](!_.contains(GoldenGlobe))

    assertResult(5)(fold.foldMap(breakingBad)(_.length))
  }

  test("are there any actors born before 1970 and don't have any nominations") {
    implicit val eqActor: Eq[Award] = Eq.fromUniversalEquals[Award]
    val foldPredicate = Getter[Actor, Award](_.nomiation) compose Prism.only[Award](None_)
    val fold =
      Fold.fromFoldable[List, TVShow] compose
        Getter[TVShow, List[Actor]](_.actors) compose
        Fold.fromFoldable[List, Actor] compose
        Fold.filter(foldPredicate)

    assertResult(true)(fold.any(tvShows)(_.birthYear < 1970))
  }

  test("fold over the elements while dropping the first 3 chars") {
    val fold = Fold.drop[List, Char](3)
    val input = "No Country for old man".toList

    assertResult("Country for old man")(fold.foldMap(input)(_.toString))
  }

  test("select the first word from each sentence in a list") {
    val fold =
      Fold.fromFoldable[List, String] compose
        words.take(1)
    val input = List("Say Anything", "My Octopus Teacher", "Name of the Rose")

    assertResult(List("Say", "My", "Name"))(fold.viewAll(input))
  }

  test("count the number of elements that come before the first occurrence of 3") {
    val fold = Fold.takeWhile[List, Int](_ < 3)

    assertResult(3)(fold.length(List(0, 1, 2, 3, 4, 5, 2)))
  }

  test("view all elements starting from the first occurrence of 3") {
    val traversal = Fold.dropWhile[List, Int](_ < 3)

    assertResult(List(3, 4, 5, 2))(traversal.viewAll(List(1, 2, 3, 4, 5, 2)))
  }
}
