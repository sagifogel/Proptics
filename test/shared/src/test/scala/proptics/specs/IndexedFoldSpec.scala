package proptics.specs

import scala.Function.const
import scala.util.Random

import cats.data.State
import cats.instances.int._
import cats.syntax.functor._
import cats.syntax.option._
import spire.std.boolean._

import proptics.instances.foldableWithIndex._
import proptics.specs.compose._
import proptics.syntax.indexedFold._
import proptics.syntax.tuple._
import proptics.{AnIndexedLens, IndexedFold, IndexedGetter, IndexedLens, IndexedTraversal}

class IndexedFoldSpec extends PropticsSuite {
  val emptyList: List[Int] = List.empty[Int]
  val ones: List[(Int, Int)] = List.fill(10)(1).zipWithIndex
  val foldable: IndexedFold[Int, Whole, Int] = IndexedFold[Int, Whole, Int](w => (w.part, 0))
  val replicated: IndexedFold[Int, Int, Int] = IndexedFold.replicate[Int, Int](10)(spire.std.int.IntAlgebra)
  val filtered: IndexedFold[Int, (Int, Int), Int] = IndexedFold.filtered[Int, Int](evenNumbers compose Tuple2._2)
  val fromFoldable: IndexedFold[Int, List[Int], Int] = IndexedFold.fromFoldable[List, Int]
  val fromFoldableWithIndex: IndexedFold[Int, List[Int], Int] = IndexedFold.fromFoldableWithIndex[List, Int, Int]
  val boolFoldable: IndexedFold[Int, List[Boolean], Boolean] = IndexedFold.fromFoldableWithIndex[List, Int, Boolean]
  val fromGetter: IndexedFold[Int, List[Int], List[Int]] = IndexedGetter[Int, List[Int], List[Int]]((_, 0)).asIndexedFold
  val unfolded: IndexedFold[Int, FoldState, (Int, Int)] = IndexedFold.unfold[Int, FoldState, (Int, Int)] { state =>
    if (state.i <= 10) (((state.i - 1, state.i), 0), state.copy(i = state.i + 1)).some else None
  }

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

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromFoldableWithIndex.sum(list) shouldEqual list.sum
      fromFoldable.sum(list) shouldEqual list.sum
      foldable.sum(whole9) shouldEqual 9
    }

    test("product") {
      fromFoldableWithIndex.product(list) shouldEqual list.product
      fromFoldableWithIndex.product(emptyList) shouldEqual 1
      fromFoldable.product(list) shouldEqual list.product
      fromFoldable.product(emptyList) shouldEqual 1
      foldable.product(whole9) shouldEqual 9
    }
  }

  test("forall") {
    fromFoldableWithIndex.forall(_._1 < 10)(list) shouldEqual true
    fromFoldableWithIndex.forall(_._1 < 10)(emptyList) shouldEqual true
    fromFoldableWithIndex.forall(_._1 > 10)(list) shouldEqual false
    fromFoldableWithIndex.forall(_._1 > 10)(emptyList) shouldEqual true
    fromFoldable.forall(_._1 < 10)(list) shouldEqual true
    fromFoldable.forall(_._1 < 10)(emptyList) shouldEqual true
    fromFoldable.forall(_._1 > 10)(list) shouldEqual false
    fromFoldable.forall(_._1 > 10)(emptyList) shouldEqual true
    foldable.forall(_._1 < 10)(whole9) shouldEqual true
    foldable.forall(_._1 > 10)(whole9) shouldEqual false
  }

  test("forall using heyting") {
    fromFoldableWithIndex.forall(list)(_._1 < 10) shouldEqual true
    fromFoldableWithIndex.forall(emptyList)(_._1 < 10) shouldEqual true
    fromFoldableWithIndex.forall(list)(_._1 > 10) shouldEqual false
    fromFoldableWithIndex.forall(emptyList)(_._1 > 10) shouldEqual true
    fromFoldable.forall(list)(_._1 < 10) shouldEqual true
    fromFoldable.forall(emptyList)(_._1 < 10) shouldEqual true
    fromFoldable.forall(list)(_._1 > 10) shouldEqual false
    fromFoldable.forall(emptyList)(_._1 > 10) shouldEqual true
    foldable.forall(whole9)(_._1 < 10) shouldEqual true
    foldable.forall(whole9)(_._1 > 10) shouldEqual false
  }

  test("and") {
    boolFoldable.and(boolList) shouldEqual false
    boolFoldable.and(boolList.map(const(true))) shouldEqual true
    boolFoldable.and(falseBoolList) shouldEqual false
  }

  test("or") {
    boolFoldable.or(boolList) shouldEqual true
    boolFoldable.or(falseBoolList) shouldEqual false
  }

  test("any") {
    fromFoldableWithIndex.any(list)(greaterThan5 compose Tuple2._1) shouldEqual true
    fromFoldableWithIndex.any(emptyList)(greaterThan10 compose Tuple2._1) shouldEqual false
    fromFoldable.any(list)(greaterThan5 compose Tuple2._1) shouldEqual true
    fromFoldable.any(emptyList)(greaterThan10 compose Tuple2._1) shouldEqual false
    foldable.any(whole9)(greaterThan5 compose Tuple2._1) shouldEqual true
  }

  test("exists") {
    fromFoldableWithIndex.exists(greaterThan5 compose Tuple2._1)(list) shouldEqual true
    fromFoldableWithIndex.exists(greaterThan10 compose Tuple2._1)(list) shouldEqual false
    fromFoldable.exists(greaterThan5 compose Tuple2._1)(list) shouldEqual true
    fromFoldable.exists(greaterThan10 compose Tuple2._1)(list) shouldEqual false
    foldable.exists(greaterThan5 compose Tuple2._1)(whole9) shouldEqual true
    foldable.exists(greaterThan10 compose Tuple2._1)(whole9) shouldEqual false
  }

  test("notExists") {
    fromFoldableWithIndex.notExists(greaterThan5 compose Tuple2._1)(list) shouldEqual false
    fromFoldableWithIndex.notExists(greaterThan10 compose Tuple2._1)(list) shouldEqual true
    fromFoldableWithIndex.notExists(greaterThan10 compose Tuple2._1)(list) shouldEqual
      !fromFoldableWithIndex.exists(greaterThan10 compose Tuple2._1)(list)
    fromFoldable.notExists(greaterThan5 compose Tuple2._1)(list) shouldEqual false
    fromFoldable.notExists(greaterThan10 compose Tuple2._1)(list) shouldEqual true
    fromFoldable.notExists(greaterThan10 compose Tuple2._1)(list) shouldEqual
      !fromFoldable.exists(greaterThan10 compose Tuple2._1)(list)
    foldable.notExists(greaterThan5 compose Tuple2._1)(whole9) shouldEqual false
    foldable.notExists(greaterThan10 compose Tuple2._1)(whole9) shouldEqual true
    foldable.notExists(greaterThan10 compose Tuple2._1)(whole9) shouldEqual !foldable.exists(greaterThan10 compose Tuple2._1)(whole9)
  }

  test("contains") {
    fromFoldableWithIndex.contains((1, 0))(list) shouldEqual true
    fromFoldableWithIndex.contains((5, 4))(list) shouldEqual true
    fromFoldableWithIndex.contains((10, 0))(list) shouldEqual false
    fromFoldable.contains((1, 1))(list) shouldEqual false
    fromFoldable.contains((1, 0))(list) shouldEqual true
    fromFoldable.contains((5, 4))(list) shouldEqual true
    fromFoldable.contains((10, 0))(list) shouldEqual false
    fromFoldable.contains((1, 1))(list) shouldEqual false
    foldable.contains((9, 0))(whole9) shouldEqual true
    foldable.contains((10, 0))(whole9) shouldEqual false
    foldable.contains((9, 1))(whole9) shouldEqual false
  }

  test("notContains") {
    fromFoldableWithIndex.notContains((1, 0))(list) shouldEqual false
    fromFoldableWithIndex.notContains((5, 4))(list) shouldEqual false
    fromFoldableWithIndex.notContains((10, 1))(list) shouldEqual true
    fromFoldableWithIndex.notContains((1, 0))(list) shouldEqual
      !fromFoldableWithIndex.contains((1, 0))(list)
    fromFoldable.notContains((1, 0))(list) shouldEqual false
    fromFoldable.notContains((5, 4))(list) shouldEqual false
    fromFoldable.notContains((10, 1))(list) shouldEqual true
    fromFoldable.notContains((1, 0))(list) shouldEqual
      !fromFoldable.contains((1, 0))(list)
    foldable.notContains((9, 0))(whole9) shouldEqual false
    foldable.notContains((10, 0))(whole9) shouldEqual true
    foldable.notContains((9, 1))(whole9) shouldEqual true
    foldable.notContains((9, 0))(whole9) shouldEqual !foldable.contains((9, 0))(whole9)
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
    fromFoldableWithIndex.asFold.foldLeft(list)(emptyList) { (ls, i) =>
      ls ++ List(i)
    } shouldEqual list

    fromFoldable.asFold.foldLeft(list)(emptyList) { (ls, i) =>
      ls ++ List(i)
    } shouldEqual list
  }

  test("reindex") {
    fromFoldableWithIndex
      .reindex(_.toString)
      .viewAll(list) shouldEqual indexedList.map(_.map(_.toString))

    fromFoldable
      .reindex(_.toString)
      .viewAll(list) shouldEqual indexedList.map(_.map(_.toString))
  }

  test("replicate") {
    replicated.viewAll(1) shouldEqual ones
    replicated.foldMap(1) { case (a, i) => if (i % 2 === 0) a else 0 } shouldEqual
      ones.filter(_._2 % 2 == 0).map(_._1).sum
  }

  test("unfold") {
    unfolded.viewAll(foldState) shouldEqual List.tabulate(10)(i => ((i, i + 1), 0))
    unfolded.length(foldState) shouldEqual 10
  }

  test("filtered") {
    val composed = unfolded <<* filtered

    composed.foldMap(foldState)(_._2) shouldEqual 0
    composed.foldMap(foldState)(_._1) shouldEqual 25
  }

  test("compose with Iso") {
    val composed = indexedFold compose iso

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with AnIso") {
    val composed = indexedFold compose anIso

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with Lens") {
    val composed = indexedFold compose lens

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with ALens") {
    val composed = indexedFold compose aLens

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with Prism") {
    val composed = indexedFold compose prism

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with APrism") {
    val composed = indexedFold compose aPrism

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with AffineTraversal") {
    val composed = indexedFold compose affineTraversal

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

  test("compose with ATraversal") {
    val composed = indexedFold compose aTraversal

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with Getter") {
    val composed = indexedFold compose getter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("to") {
    indexedFold.to(_ + 1).fold(8) shouldEqual 9
  }

  test("compose with Fold") {
    val composed = indexedFold compose fold

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

  test("element") {
    fromFoldableWithIndex.elementAt(1).preview(list) shouldEqual 2.some
    fromFoldable.elementAt(1).preview(list) shouldEqual 2.some
  }

  test("has") {
    IndexedFold.has(fromFoldable)(List(1, 2, 3)) shouldEqual true
    IndexedFold.has(fromFoldable)(List.empty[Int]) shouldEqual false
  }
}
