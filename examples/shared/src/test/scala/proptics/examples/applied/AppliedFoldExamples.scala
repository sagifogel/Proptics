package proptics.examples.applied

import cats.Eq
import cats.data.NonEmptyList
import cats.instances.all._
import cats.kernel.Order
import cats.syntax.all._

import proptics.examples._
import proptics.instances.nonEmptyCons._
import proptics.instances.reverse._
import proptics.specs.PropticsSuite
import proptics.syntax.all._

class AppliedFoldExamples extends PropticsSuite {
  test("fold over an option") {
    val list = "value".some.foldable.viewAll

    assertResult(List("value"))(list)
  }

  test("fold over a tuple") {
    val list = (9, "value").foldable.viewAll
    assertResult(List("value"))(list)
  }

  test("compose folds") {
    val composed =
      List((0, "Govan"), (1, "Abassi"), (2, "Gambale")).foldable.andThenFold.toChars.andThenFold

    assertResult("GovanAbassiGambale")(composed.viewAll.mkString)
  }

  test("parse both elements of the tuple") {
    val both1 = ("1", "2").bifold_[Int].foldMap(parseInt)
    val both2 = ("NaN", "2").bifold_[Int].foldMap(parseInt)

    assertResult(3.some)(both1)
    assertResult(2.some)(both2)
  }

  test("get the name of the oldest actor") {
    implicit val actorsOrder: Order[Actor] = Order.by(_.birthYear)
    val actors = tvShows.foldable.focus(_.actors).andThenFold

    assertResult("Jonathan Banks".some)(actors.minimum.map(_.name))
  }

  test("actors who participate in more than one show") {
    val actors = tvShows.foldable.focus(_.actors.map(_.name -> 1).toMap)
    val expected = List("Jonathan Banks", "Giancarlo Esposito", "Bob Odenkirk").toSet
    val actorsInBothShows =
      actors.fold.collect { case (actor, numOShows) if numOShows > 1 => actor }.toSet

    assertResult(expected)(actorsInBothShows)
  }

  test("View all actors that their first name starts with the letter 'A'") {
    val fold =
      tvShows.foldable
        .focus(_.actors)
        .andThenFold
        .focus(_.name)
        .filter(_.startsWith("A"))
        .takeWords(1)

    assertResult(List("Aaron", "Anna"))(fold.viewAll)
  }

  test("using fold as a predicate, count the number of awards of all actors that were nominated for golden globe but did not win") {
    implicit val eqActor: Eq[Award] = Eq.fromUniversalEquals[Award]
    val fold =
      breakingBad.actors.foldable
        .onlyWhen(_.nomination, GoldenGlobe)
        .focus(_.awards)
        .filterNot(_.contains(GoldenGlobe))

    assertResult(5)(fold.foldMap(_.length))
  }

  test("fold over the elements while dropping the first 3 chars") {
    val fold = "No Country for old man".toList.foldable.drop(3)

    assertResult("Country for old man")(fold.foldMap(_.toString))
  }

  test("select the first word from each sentence in a list") {
    val input = List("Say Anything", "My Octopus Teacher", "Name of the Rose")
    val fold = input.foldable.takeWords(1)

    assertResult(List("Say", "My", "Name"))(fold.viewAll)
  }

  test("count the number of elements that come before the first occurrence of 3") {
    val fold = List(0, 1, 2, 3, 4, 5, 2).foldable.takeWhile(_ < 3)

    assertResult(3)(fold.length)
  }

  test("view all elements starting from the first occurrence of 3") {
    val fold = List(1, 2, 3, 4, 5, 2).foldable.dropWhile(_ < 3)

    assertResult(List(3, 4, 5, 2))(fold.viewAll)
  }

  test("view all elements of a list in a reversed order") {
    val fold = List(List(1, 2, 3, 4)).foldable.reverse

    assertResult(List(List(4, 3, 2, 1)))(fold.viewAll)
  }

  test("swap the elements of a tuple") {
    val list = List((1, "First"), (2, "Second"))
    val nel = NonEmptyList.fromList(list)
    val foldable = nel.foldable.head.swap

    assertResult(List(("First", 1)))(foldable.viewAll)
  }

  test("swap the elements of an either") {
    val list = List(Right("First"), Left(2))
    val nel = NonEmptyList.fromList(list)
    val foldable = nel.foldable.head.swap

    assertResult(List(Left("First")))(foldable.viewAll)
  }

  test("extract the content of Some elements in a List") {
    val someValues =
      List(Some("Some"), None, Some(" Of These Days")).foldable.some

    assertResult("Some Of These Days")(someValues.view)
  }

  test("count the number of None elements in a List") {
    val someValues =
      List(Some("some"), None, Some(" Of These Days"), None, None).foldable.none

    assertResult(3)(someValues.length)
  }

  test("extract the content of Right elements in a List") {
    val rightValues =
      List(Right("That's"), Left(true), Right(" right"), Left(false)).foldable.right

    assertResult("That's right")(rightValues.view)
  }

  test("extract the content of Left elements in a List") {
    val leftValues =
      List(Right(true), Left("That's"), Right(true), Left(" wrong")).foldable.left

    assertResult("That's wrong")(leftValues.view)
  }
}
