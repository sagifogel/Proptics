package proptics.examples

import cats.kernel.Eq
import spire.std.boolean._
import spire.std.int._

import proptics.specs.PropticsSuite
import proptics.{Fold, Getter, Prism}

trait FoldCompatSuite extends PropticsSuite {
  test("sum all number of episodes") {
    val numberOfEpisodes =
      Fold.fromFoldable[List, TVShow] andThen
        Fold[TVShow, Int](_.numEpisodes)

    assertResult(125)(numberOfEpisodes.sum(tvShows))
  }

  test("are there any actors born before 1970 and don't have any nominations") {
    implicit val eqActor: Eq[Award] = Eq.fromUniversalEquals[Award]
    val foldPredicate = Getter[Actor, Award](_.nomiation) andThen Prism.only[Award](None_)
    val fold =
      Fold.fromFoldable[List, TVShow] andThen
        Getter[TVShow, List[Actor]](_.actors) andThen
        Fold.fromFoldable[List, Actor] andThen
        Fold.filter(foldPredicate)

    assertResult(true)(fold.any(tvShows)(_.birthYear < 1970))
  }
}
