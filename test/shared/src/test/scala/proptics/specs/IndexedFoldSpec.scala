package proptics.specs

import scala.Function.const
import scala.util.Random

import cats.data.State
import cats.instances.int._
import cats.syntax.option._

import proptics.instances.foldableWithIndex._
import proptics.specs.compose._
import proptics.syntax.indexedFold._
import proptics.syntax.tuple._
import proptics.{AnIndexedLens, IndexedFold, IndexedGetter, IndexedLens, IndexedTraversal}

class IndexedFoldSpec extends IndexedFoldCompatSuite {
  val ones: List[(Int, Int)] = List.fill(10)(1).zipWithIndex
  val foldable: IndexedFold[Int, Whole, Int] = IndexedFold[Int, Whole, Int](w => (w.part, 0))
  val filtered: IndexedFold[Int, (Int, Int), Int] = IndexedFold.filtered[Int, Int](evenNumbers compose Tuple2._2)
  val fromFoldable: IndexedFold[Int, List[Int], Int] = IndexedFold.fromFoldable[List, Int]
  val fromFoldableWithIndex: IndexedFold[Int, List[Int], Int] = IndexedFold.fromFoldableWithIndex[List, Int, Int]
  val boolFoldable: IndexedFold[Int, List[Boolean], Boolean] = IndexedFold.fromFoldableWithIndex[List, Int, Boolean]
  val fromGetter: IndexedFold[Int, List[Int], List[Int]] = IndexedGetter[Int, List[Int], List[Int]]((_, 0)).asIndexedFold

  test("viewAll") {
    fromFoldableWithIndex.viewAll(list) shouldEqual indexedList
    fromFoldable.viewAll(list) shouldEqual indexedList
    fromFoldableWithIndex.viewAll(emptyList) shouldEqual emptyIndexedList
    fromFoldable.viewAll(emptyList) shouldEqual emptyIndexedList
    foldable.viewAll(whole9) shouldEqual List((whole9.part, 0))
  }

  test("preview") {
    fromFoldableWithIndex.preview(list) shouldEqual (1, 0).some
    fromFoldableWithIndex.preview(emptyList) shouldEqual None
    fromFoldable.preview(list) shouldEqual (1, 0).some
    fromFoldable.preview(emptyList) shouldEqual None
    foldable.preview(whole9) shouldEqual (9, 0).some
  }

  test("foldMap") {
    fromFoldableWithIndex.foldMap(list)(_._1) shouldEqual list.sum
    fromFoldableWithIndex.foldMap(list)(List(_)) shouldEqual indexedList
    fromFoldableWithIndex.foldMap(emptyList)(_._1) shouldEqual 0
    fromFoldableWithIndex.foldMap(emptyList)(List(_)) shouldEqual emptyIndexedList
    fromFoldable.foldMap(list)(_._1) shouldEqual list.sum
    fromFoldable.foldMap(list)(List(_)) shouldEqual indexedList
    fromFoldable.foldMap(emptyList)(_._1) shouldEqual 0
    fromFoldable.foldMap(emptyList)(List(_)) shouldEqual emptyIndexedList
    foldable.foldMap(whole9)(_._1) shouldEqual 9
  }

  test("fold") {
    fromFoldableWithIndex.fold(list) shouldEqual list.sum
    fromFoldableWithIndex.fold(list) shouldEqual fromFoldable.view(list)
    fromFoldableWithIndex.fold(emptyList) shouldEqual 0
    fromFoldableWithIndex.fold(emptyList) shouldEqual fromFoldable.view(emptyList)
    foldable.fold(whole9) shouldEqual 9
    foldable.fold(whole9) shouldEqual foldable.view(whole9)
    fromGetter.fold(list) shouldEqual list
  }

  test("foldRight") {
    fromFoldableWithIndex.foldRight(list)(0)(_._1 + _) shouldEqual list.sum
    fromFoldableWithIndex.foldRight(list)(0)(_._1 + _) should be > 0
    fromFoldableWithIndex.foldRight(list)(emptyList)(_._1 :: _) shouldEqual list
    fromFoldableWithIndex.foldRight(emptyList)(0)(_._1 + _) shouldEqual 0
    fromFoldableWithIndex.foldRight(emptyList)(0)(_._1 - _) shouldEqual 0
    fromFoldable.foldRight(list)(0)(_._1 + _) shouldEqual list.sum
    fromFoldable.foldRight(list)(0)(_._1 + _) should be > 0
    fromFoldable.foldRight(list)(emptyList)(_._1 :: _) shouldEqual list
    fromFoldable.foldRight(emptyList)(0)(_._1 + _) shouldEqual 0
    fromFoldable.foldRight(emptyList)(0)(_._1 - _) shouldEqual 0
    foldable.foldRight(whole9)(1)(_._1 + _) shouldEqual 10
    foldable.foldRight(whole9)(1)(_._1 - _) shouldEqual 8
  }

  test("foldLeft") {
    fromFoldableWithIndex.foldLeft(list)(0)(_ + _._1) shouldEqual list.sum
    fromFoldableWithIndex.foldLeft(list)(0)(_ + _._1) should be > 0
    fromFoldableWithIndex.foldLeft(list)(emptyList)((r, a) => a._1 :: r) shouldEqual list.reverse
    fromFoldableWithIndex.foldLeft(emptyList)(0)(_ + _._1) shouldEqual 0
    fromFoldableWithIndex.foldLeft(emptyList)(0)(_ - _._1) shouldEqual 0
    fromFoldable.foldLeft(list)(0)(_ + _._1) shouldEqual list.sum
    fromFoldable.foldLeft(list)(0)(_ + _._1) should be > 0
    fromFoldable.foldLeft(list)(emptyList)((r, a) => a._1 :: r) shouldEqual list.reverse
    fromFoldable.foldLeft(emptyList)(0)(_ + _._1) shouldEqual 0
    fromFoldable.foldLeft(emptyList)(0)(_ - _._1) shouldEqual 0
    foldable.foldLeft(whole9)(1)(_ + _._1) shouldEqual 10
    foldable.foldLeft(whole9)(1)(_ - _._1) shouldEqual -8
  }

  test("isEmpty") {
    fromFoldableWithIndex.isEmpty(list) shouldEqual false
    fromFoldableWithIndex.isEmpty(emptyList) shouldEqual true
    fromFoldable.isEmpty(list) shouldEqual false
    fromFoldable.isEmpty(emptyList) shouldEqual true
    foldable.isEmpty(whole9) shouldEqual false
  }

  test("nonEmpty") {
    fromFoldableWithIndex.nonEmpty(list) shouldEqual true
    fromFoldableWithIndex.nonEmpty(emptyList) shouldEqual false
    fromFoldableWithIndex.nonEmpty(list) shouldEqual !fromFoldableWithIndex.isEmpty(list)
    fromFoldable.nonEmpty(list) shouldEqual true
    fromFoldable.nonEmpty(emptyList) shouldEqual false
    fromFoldable.nonEmpty(list) shouldEqual !fromFoldable.isEmpty(list)
    foldable.nonEmpty(whole9) shouldEqual true
    foldable.nonEmpty(whole9) shouldEqual !foldable.isEmpty(whole9)
  }

  test("length") {
    fromFoldableWithIndex.length(list) shouldEqual indexedList.length
    fromFoldableWithIndex.length(emptyList) shouldEqual 0
    foldable.length(whole9) shouldEqual 1
  }

  test("find") {
    fromFoldableWithIndex.find(greaterThan5 compose Tuple2._1)(list) shouldEqual list.find(greaterThan5).map((_, 5))
    fromFoldableWithIndex.find(greaterThan10 compose Tuple2._1)(list) shouldEqual None
    fromFoldable.find(greaterThan5 compose Tuple2._1)(list) shouldEqual list.find(greaterThan5).map((_, 5))
    fromFoldable.find(greaterThan10 compose Tuple2._1)(list) shouldEqual None
    foldable.find(greaterThan5 compose Tuple2._1)(whole9) shouldEqual (9, 0).some
    foldable.find(greaterThan10 compose Tuple2._1)(whole9) shouldEqual None
  }

  test("first") {
    fromFoldableWithIndex.first(list) shouldEqual (list.head, 0).some
    fromFoldableWithIndex.first(emptyList) shouldEqual None
    fromFoldable.first(list) shouldEqual (list.head, 0).some
    fromFoldable.first(emptyList) shouldEqual None
    foldable.first(whole9) shouldEqual (9, 0).some
  }

  test("last") {
    fromFoldableWithIndex.last(list) shouldEqual (list.last, list.length - 1).some
    fromFoldableWithIndex.last(emptyList) shouldEqual None
    fromFoldable.last(list) shouldEqual (list.last, list.length - 1).some
    fromFoldable.last(emptyList) shouldEqual None
    foldable.last(whole9) shouldEqual (9, 0).some
  }

  {
    val ev = catsKernelStdOrderForInt

    test("minimum") {
      fromFoldableWithIndex.minimum(Random.shuffle(list))(ev) shouldEqual list.head.some
      fromFoldableWithIndex.minimum(emptyList)(ev) shouldEqual None
      fromFoldable.minimum(Random.shuffle(list))(ev) shouldEqual list.head.some
      fromFoldable.minimum(emptyList)(ev) shouldEqual None
      foldable.minimum(whole9)(ev) shouldEqual 9.some
    }

    test("maximum") {
      fromFoldableWithIndex.maximum(Random.shuffle(list))(ev) shouldEqual list.last.some
      fromFoldableWithIndex.maximum(emptyList)(ev) shouldEqual None
      fromFoldable.maximum(Random.shuffle(list))(ev) shouldEqual list.last.some
      fromFoldable.maximum(emptyList)(ev) shouldEqual None
      foldable.maximum(whole9)(ev) shouldEqual 9.some
    }
  }

  test("toArray") {
    fromFoldableWithIndex.toArray(list) shouldEqual list.toArray
    fromFoldable.toArray(list) shouldEqual list.toArray
    foldable.toArray(whole9) shouldEqual Array(9)
  }

  test("toList") {
    fromFoldableWithIndex.toList(list) shouldEqual list
    fromFoldable.toList(list) shouldEqual list
    foldable.toList(whole9) shouldEqual List(9)
  }

  test("use") {
    implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](1)

    fromFoldableWithIndex.use.runA(list).value shouldEqual indexedList
    fromFoldable.use.runA(list).value shouldEqual indexedList
    foldable.use.runA(whole9).value shouldEqual List((9, 0))
  }

  test("asFold") {
    fromFoldableWithIndex.asFold.foldLeft(list)(emptyList)((ls, i) => ls ++ List(i)) shouldEqual list
    fromFoldable.asFold.foldLeft(list)(emptyList)((ls, i) => ls ++ List(i)) shouldEqual list
  }

  test("reindex") {
    fromFoldableWithIndex
      .reindex(_.toString)
      .viewAll(list) shouldEqual indexedList.map { case (a, i) => (a, i.toString) }

    fromFoldable
      .reindex(_.toString)
      .viewAll(list) shouldEqual indexedList.map { case (a, i) => (a, i.toString) }
  }

  test("compose with Iso") {
    val composed = indexedFold compose iso

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with Iso") {
    val composed = indexedFold andThen iso

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with AnIso") {
    val composed = indexedFold compose anIso

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with AnIso") {
    val composed = indexedFold andThen anIso

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with Lens") {
    val composed = indexedFold compose lens

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with Lens") {
    val composed = indexedFold andThen lens

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with ALens") {
    val composed = indexedFold compose aLens

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with ALens") {
    val composed = indexedFold andThen aLens

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with Prism") {
    val composed = indexedFold compose prism

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with Prism") {
    val composed = indexedFold andThen prism

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with APrism") {
    val composed = indexedFold compose aPrism

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with APrism") {
    val composed = indexedFold andThen aPrism

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with AffineTraversal") {
    val composed = indexedFold compose affineTraversal

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with AffineTraversal") {
    val composed = indexedFold andThen affineTraversal

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with AnAffineTraversal") {
    val composed = indexedFold compose anAffineTraversal

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with Traversal") {
    val composed = indexedFold compose traversal

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with Traversal") {
    val composed = indexedFold andThen traversal

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with ATraversal") {
    val composed = indexedFold compose aTraversal

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with ATraversal") {
    val composed = indexedFold andThen aTraversal

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with Getter") {
    val composed = indexedFold compose getter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with Getter") {
    val composed = indexedFold andThen getter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("focus") {
    indexedFold.focus(_ + 1).fold(8) shouldEqual 9
  }

  test("compose with Fold") {
    val composed = indexedFold compose fold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with Fold") {
    val composed = indexedFold andThen fold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedLens with right index") {
    val composed = indexedFold *>> IndexedLens[Int, Int, Int]((_, 1))(_ => identity)

    composed.foldMap(9)(_._2) shouldEqual 1
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedLens with left index") {
    val composed = indexedFold <<* IndexedLens[Int, Int, Int]((_, 1))(_ => identity)

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with AnIndexedLens with right index") {
    val composed = indexedFold *>> AnIndexedLens[Int, Int, Int]((_, 1))(_ => identity)

    composed.foldMap(9)(_._2) shouldEqual 1
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with AnIndexedLens with left index") {
    val composed = indexedFold <<* AnIndexedLens[Int, Int, Int]((_, 1))(_ => identity)

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedTraversal with right index") {
    val composed = indexedFold *>> IndexedTraversal[Int, Int, Int]((_, 1))(const(identity))

    composed.foldMap(9)(_._2) shouldEqual 1
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedTraversal with left index") {
    val composed = indexedFold <<* IndexedTraversal[Int, Int, Int]((_, 1))(const(identity))

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedGetter with right index") {
    val composed = indexedFold *>> IndexedGetter[Int, Int, Int]((_, 1))

    composed.foldMap(9)(_._2) shouldEqual 1
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedGetter with left index") {
    val composed = indexedFold <<* IndexedGetter[Int, Int, Int]((_, 1))

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("toWithIndex") {
    indexedFold.toWithIndex[Int, Int](i => (i + 1, 0)).foldMap(8)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold with right index") {
    val composed = indexedFold *>> IndexedFold[Int, Int, Int]((_, 1))

    composed.foldMap(9)(_._2) shouldEqual 1
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold with left index") {
    val composed = indexedFold <<* IndexedFold[Int, Int, Int]((_, 1))

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("filterByIndex") {
    fromFoldableWithIndex.filterByIndex(_ < 3).viewAll(list) shouldEqual indexedList.take(3)
    fromFoldable.filterByIndex(_ < 3).viewAll(list) shouldEqual indexedList.take(3)
  }

  test("index") {
    fromFoldableWithIndex.index(1).preview(list) shouldEqual 2.some
    fromFoldable.index(1).preview(list) shouldEqual 2.some
  }

  test("has") {
    IndexedFold.has(fromFoldable)(List(1, 2, 3)) shouldEqual true
    IndexedFold.has(fromFoldable)(List.empty[Int]) shouldEqual false
  }

  test("implicit cast to from IndexedLens") {
    val indexedFoldFromIndexedLens: IndexedFold[Int, Int, Int] = indexedLens

    indexedFoldFromIndexedLens.foldMap(9)(identity) shouldEqual ((9, 0))
  }

  test("implicit cast to AnIndexedLens") {
    val indexedFoldFromAnIndexedLens: IndexedFold[Int, Int, Int] = anIndexedLens

    indexedFoldFromAnIndexedLens.foldMap(9)(identity) shouldEqual ((9, 0))
  }

  test("implicit cast to IndexedTraversal") {
    val indexedFoldFromIndexedTraversal: IndexedFold[Int, Int, Int] = indexedTraversal

    indexedFoldFromIndexedTraversal.foldMap(9)(identity) shouldEqual ((9, 0))
  }

  test("implicit cast to IndexedGetter") {
    val indexedFoldFromIndexedGetter: IndexedFold[Int, Int, Int] = indexedGetter

    indexedFoldFromIndexedGetter.foldMap(9)(identity) shouldEqual ((9, 0))
  }
}
