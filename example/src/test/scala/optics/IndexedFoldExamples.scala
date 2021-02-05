package optics

import cats.syntax.option._
import proptics.Traversal.both
import proptics.instances.foldableWithIndex._
import proptics.instances.index.{index, _}
import proptics.specs.PropticsSuite
import proptics.std.tuple._1
import proptics.syntax.indexedFold._
import proptics.{IndexedFold, IndexedFold_, IndexedLens}
import spire.std.int._

class IndexedFoldExamples extends PropticsSuite {
  val languages: List[Language] = List(
    Language("Haskell", "Simon Peyton Jones"),
    Language("Clojure", "Rich Hickey"),
    Language("Scala", "Martin Oderski"),
    Language("Erlang", "Joe Armstrong"),
    Language("Haskell", "Philip Wadler")
  )

  test("use fromFoldable for Foldable with Int indices") {
    val indexedFold: IndexedFold[Int, List[String], String] =
      IndexedFold.fromFoldable[List, String]

    val folded = indexedFold.foldMap(List("hello", "world")) { case (str, i) => List(s"$str -> $i") }
    assertResult(List("hello -> 0", "world -> 1"))(folded)
  }

  test("get a list of all elements and their key") {
    val indexedFold: IndexedFold[Int, List[Int], Int] =
      IndexedFold.fromFoldable[List, Int]

    val list = List(10, 20, 30)
    assertResult(List((10, 0), (20, 1), (30, 2)))(indexedFold.viewAll(list))
  }

  test("filter only characters which are in an even index position") {
    val fold = IndexedFold.fromFoldable[List, Char].filterByIndex(even)
    val folded = fold.toList(('a' to 'z').toList)

    assertResult("acegikmoqsuwy")(folded.mkString)
  }

  test("focus on an element at a specific index") {
    val fold = IndexedFold.fromFoldable[List, Char].elementAt(10)

    assertResult('k'.some)(fold.preview(('a' to 'z').toList))
  }

  test("focus on elements that reside at an even index and are greater than 5") {
    val fold = IndexedFold.fromFoldable[List, Int].filter { case (a, i) => even(i) & a > 5 }

    assertResult(List((7, 6), (9, 8)))(fold.viewAll(List.range(1, 10)))
  }

  test("map over the indices of an optic") {
    val input = List("A", "B", "C")
    val expected = List(("A", 0), ("B", 10), ("C", 20))
    val fold =
      IndexedFold
        .fromFoldable[List, String]
        .reindex(_ * 10)

    assertResult(expected)(fold.viewAll(input))
  }

  test("unindex an IndexedFold") {
    val input = List("A", "B", "C")
    val fold =
      IndexedFold.fromFoldable[List, String].unIndex

    assertResult(input)(fold.viewAll(input))
  }

  test("summon and use an instance of FoldableWithIndex for a Map") {
    val map = Map[Int, String](0 -> "A", 1 -> "1", 2 -> "2")
    val fold = IndexedFold_.fromFoldableWithIndex[Map[Int, *], Int, String, Int]
    val foldedList = fold.foldMap(map) { case (key, value) =>
      List(parseInt(key).map(_ + value))
    }

    assertResult(List(None, 2.some, 4.some))(foldedList)
  }

  test("compose with other optic and taking the right optic's indices") {
    val map = Map("Scala" -> List("Some", "None"), "Haskell" -> List("Just", "Nothing"))
    val mapFold = IndexedFold.fromFoldableWithIndex[Map[String, *], String, List[String]]
    val listFold = IndexedFold.fromFoldableWithIndex[List, Int, String]
    val composedWithRightIndex: IndexedFold[Int, Map[String, List[String]], String] =
      mapFold *>> listFold
    val expected = List(("Some", 0), ("None", 1), ("Just", 0), ("Nothing", 1))

    assertResult(expected)(composedWithRightIndex.viewAll(map))
  }

  test("compose with other optic and taking self indices") {
    val map = Map("Scala" -> List("Some", "None"), "Haskell" -> List("Just", "Nothing"))
    val mapFoldable = IndexedFold.fromFoldableWithIndex[Map[String, *], String, List[String]]
    val listFoldable = IndexedFold.fromFoldableWithIndex[List, Int, String]
    val composedWithLeftIndex = mapFoldable <<* listFoldable
    val expected = List(("Some", "Scala"), ("None", "Scala"), ("Just", "Haskell"), ("Nothing", "Haskell"))

    assertResult(expected)(composedWithLeftIndex.viewAll(map))
  }

  test("compose with non indexed optic") {
    val map = Map("Scala" -> (("Some", "None")), "Haskell" -> (("Just", "Nothing")))
    val mapFold =
      IndexedFold.fromFoldableWithIndex[Map[String, *], String, (String, String)] compose
        _1[String, String]

    val expected = List(("Some", "Scala"), ("Just", "Haskell"))

    assertResult(expected)(mapFold.viewAll(map))
  }

  test("union all key value pairs from a tuple of maps") {
    val tupledMaps = (Map("a" -> 1, "b" -> 2), Map("c" -> 3, "d" -> 4))
    val indexedTraversal: IndexedFold[String, Map[String, Int], Int] =
      IndexedFold.fromFoldableWithIndex[Map[String, *], String, Int]
    val mapTraversal = both[(*, *), Map[String, Int]] compose indexedTraversal
    val expected = List((1, "a"), (2, "b"), (3, "c"), (4, "d"))

    assertResult(expected)(mapTraversal.viewAll(tupledMaps))
  }

  test("calculate total number of commits for a specific repo in the past week") {
    val fold =
      (IndexedFold.fromFoldableWithIndex[Map[String, *], String, Map[String, Int]] *>>
        IndexedFold.fromFoldableWithIndex[Map[String, *], String, Int])
        .elementAt("repo A")

    assertResult(33)(fold.sum(commits))
  }

  test("list out the number of commits for each day for a specific repo in the past week") {
    val expected = List((10, "Sunday"), (15, "Monday"), (5, "Wednesday"), (3, "Friday"))
    val traversal =
      IndexedFold.fromFoldableWithIndex[Map[String, *], String, Map[String, Int]] compose
        index[Map[String, Int], String, Int]("repo A")

    assertResult(expected)(traversal.viewAll(commits))
  }

  test("fold over an option using `fromFoldableWithIndex` instance") {
    val indexedFold: IndexedFold[Unit, Option[String], String] =
      IndexedFold.fromFoldableWithIndex[Option, Unit, String]

    assertResult(List(("value", ())))(indexedFold.viewAll(Some("value")))
  }

  test("using an IndexedLens as an IndexedFold") {
    val languages = List(
      Language("Haskell", "Simon Peyton Jones"),
      Language("Clojure", "Rich Hickey"),
      Language("Scala", "Martin Oderski"),
      Language("Erlang", "Joe Armstrong"),
      Language("Haskell", "Philip Wadler")
    )

    // implicit cast from [[IndexedLens_]] to [[IndexedFold_]]
    val designer: IndexedFold[String, Language, String] =
      IndexedLens[String, Language, String](l => (l.designer, l.name)) { lang => designer =>
        lang.copy(designer = designer)
      }
    val composed =
      IndexedFold.fromFoldable[List, Language] *>> designer

    assertResult(languages.map(_.designer))(composed.toList(languages))
  }
}
