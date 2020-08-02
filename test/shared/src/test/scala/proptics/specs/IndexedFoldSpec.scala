package proptics.specs

import cats.instances.int.catsKernelStdOrderForInt
import cats.data.State
import cats.instances.int._
import cats.instances.list._
import cats.syntax.option._
import proptics.syntax.tuple._
import proptics.{IndexedFold, IndexedFold_}
import spire.std.boolean._
import spire.std.int._

import scala.util.Random

class IndexedFoldSpec extends PropticsSuite {
  val ones: List[(Int, Int)] = List.fill(10)(1).zipWithIndex.map(_.swap)
  val boolIndexedList: List[(Int, Boolean)] = boolList.zipWithIndex.map(_.swap)
  val replicated: IndexedFold[Int, Int, Int] = IndexedFold.replicate[Int, Int](10)
  val falseBoolIndexedList: List[(Int, Boolean)] = falseBoolList.zipWithIndex.map(_.swap)
  val foldable: IndexedFold[Int, Whole, Int] = IndexedFold[Int, Whole, Int](w => (0, w.part))
  val filtered: IndexedFold[Int, (Int, Int), Int] = IndexedFold.filtered[Int, Int](evenNumbers compose Tuple2._2)
  val fromFoldable: IndexedFold_[Int, List[(Int, Int)], (Int, Int), Int, Int] = IndexedFold.fromFoldable[List, Int, Int]
  val boolFoldable: IndexedFold_[Int, List[(Int, Boolean)], (Int, Boolean), Boolean, Boolean] =
    IndexedFold.fromFoldable[List, Int, Boolean]
  val unfolded: IndexedFold[Int, FoldState, (Int, Int)] = IndexedFold.unfold[Int, FoldState, (Int, Int)] { state =>
    if (state.i <= 10) ((0, (state.i - 1, state.i)), state.copy(i = state.i + 1)).some else None
  }

  test("viewAll") {
    fromFoldable.viewAll(indexedList) shouldEqual indexedList
    fromFoldable.viewAll(emptyIndexedList) shouldEqual emptyIndexedList
    foldable.viewAll(whole9) shouldEqual List((0, whole9.part))
  }

  test("preview") {
    fromFoldable.preview(indexedList) shouldEqual (0, 1).some
    fromFoldable.preview(emptyIndexedList) shouldEqual None
    foldable.preview(whole9) shouldEqual (0, 9).some
  }

  test("foldMap") {
    fromFoldable.foldMap(indexedList)(_._2) shouldEqual list.sum
    fromFoldable.foldMap(indexedList)(List(_)) shouldEqual indexedList
    fromFoldable.foldMap(emptyIndexedList)(_._2) shouldEqual 0
    fromFoldable.foldMap(emptyIndexedList)(List(_)) shouldEqual emptyIndexedList
    foldable.foldMap(whole9)(_._2) shouldEqual 9
  }

  test("foldr") {
    fromFoldable.foldr(indexedList)(0)(_._2 + _) shouldEqual list.sum
    fromFoldable.foldr(indexedList)(0)(_._2 + _) should be > 0
    fromFoldable.foldr(indexedList ++ List((indexedList.length, 20)))(0)(_._2 - _) should be > 0
    fromFoldable.foldr(emptyIndexedList)(0)(_._2 + _) shouldEqual 0
    fromFoldable.foldr(emptyIndexedList)(0)(_._2 - _) shouldEqual 0
    foldable.foldr(whole9)(1)(_._2 + _) shouldEqual 10
    foldable.foldr(whole9)(1)(_._2 - _) shouldEqual 8
  }

  test("foldl") {
    fromFoldable.foldl(indexedList)(0)(_ + _._2) shouldEqual list.sum
    fromFoldable.foldl(indexedList)(0)(_ + _._2) should be > 0
    fromFoldable.foldl(indexedList ++ List((indexedList.length, 20)))(0)(_ - _._2) should be < 0
    fromFoldable.foldl(emptyIndexedList)(0)(_ + _._2) shouldEqual 0
    fromFoldable.foldl(emptyIndexedList)(0)(_ - _._2) shouldEqual 0
    foldable.foldl(whole9)(1)(_ + _._2) shouldEqual 10
    foldable.foldl(whole9)(1)(_ - _._2) shouldEqual -8
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromFoldable.sum(indexedList) shouldEqual list.sum
      foldable.sum(whole9) shouldEqual 9
    }

    test("product") {
      fromFoldable.product(indexedList) shouldEqual list.product
      fromFoldable.product(emptyIndexedList) shouldEqual 1
      foldable.product(whole9) shouldEqual 9
    }
  }

  test("forall") {
    fromFoldable.forall(_._2 < 10)(indexedList) shouldEqual true
    fromFoldable.forall(_._2 < 10)(emptyIndexedList) shouldEqual true
    fromFoldable.forall(_._2 > 10)(indexedList) shouldEqual false
    fromFoldable.forall(_._2 > 10)(emptyIndexedList) shouldEqual true
    foldable.forall(_._2 < 10)(whole9) shouldEqual true
    foldable.forall(_._2 > 10)(whole9) shouldEqual false
  }

  test("forall using heyting") {
    fromFoldable.forall(indexedList)(_._2 < 10) shouldEqual true
    fromFoldable.forall(emptyIndexedList)(_._2 < 10) shouldEqual true
    fromFoldable.forall(indexedList)(_._2 > 10) shouldEqual false
    fromFoldable.forall(emptyIndexedList)(_._2 > 10) shouldEqual true
    foldable.forall(whole9)(_._2 < 10) shouldEqual true
    foldable.forall(whole9)(_._2 > 10) shouldEqual false
  }

  test("and") {
    boolFoldable.and(boolIndexedList) shouldEqual false
    boolFoldable.and(boolIndexedList.map { case (i, _) => (i, true) }) shouldEqual true
    boolFoldable.and(falseBoolIndexedList) shouldEqual false
  }

  test("or") {
    boolFoldable.or(boolIndexedList) shouldEqual true
    boolFoldable.or(falseBoolIndexedList) shouldEqual false
  }

  test("any") {
    fromFoldable.any(indexedList)(greaterThan5 compose Tuple2._2) shouldEqual true
    fromFoldable.any(emptyIndexedList)(greaterThan10 compose Tuple2._2) shouldEqual false
    foldable.any(whole9)(greaterThan5 compose Tuple2._2) shouldEqual true
  }

  test("exists") {
    fromFoldable.exists(greaterThan5 compose Tuple2._2)(indexedList) shouldEqual true
    fromFoldable.exists(greaterThan10 compose Tuple2._2)(indexedList) shouldEqual false
    foldable.exists(greaterThan5 compose Tuple2._2)(whole9) shouldEqual true
    foldable.exists(greaterThan10 compose Tuple2._2)(whole9) shouldEqual false
  }

  test("notExists") {
    fromFoldable.notExists(greaterThan5 compose Tuple2._2)(indexedList) shouldEqual false
    fromFoldable.notExists(greaterThan10 compose Tuple2._2)(indexedList) shouldEqual true
    fromFoldable.notExists(greaterThan10 compose Tuple2._2)(indexedList) shouldEqual
      !fromFoldable.exists(greaterThan10 compose Tuple2._2)(indexedList)
    foldable.notExists(greaterThan5 compose Tuple2._2)(whole9) shouldEqual false
    foldable.notExists(greaterThan10 compose Tuple2._2)(whole9) shouldEqual true
    foldable.notExists(greaterThan10 compose Tuple2._2)(whole9) shouldEqual !foldable.exists(greaterThan10 compose Tuple2._2)(whole9)
  }

  test("contains") {
    fromFoldable.contains(indexedList)((0, 1)) shouldEqual true
    fromFoldable.contains(indexedList)((4, 5)) shouldEqual true
    fromFoldable.contains(indexedList)((0, 10)) shouldEqual false
    fromFoldable.contains(indexedList)((1, 1)) shouldEqual false
    foldable.contains(whole9)((0, 9)) shouldEqual true
    foldable.contains(whole9)((0, 10)) shouldEqual false
    foldable.contains(whole9)((1, 9)) shouldEqual false
  }

  test("notContains") {
    fromFoldable.notContains(indexedList)((0, 1)) shouldEqual false
    fromFoldable.notContains(indexedList)((4, 5)) shouldEqual false
    fromFoldable.notContains(indexedList)((1, 10)) shouldEqual true
    fromFoldable.notContains(indexedList)((0, 1)) shouldEqual
      !fromFoldable.contains(indexedList)((0, 1))
    foldable.notContains(whole9)((0, 9)) shouldEqual false
    foldable.notContains(whole9)((0, 10)) shouldEqual true
    foldable.notContains(whole9)((1, 9)) shouldEqual true
    foldable.notContains(whole9)((0, 9)) shouldEqual !foldable.contains(whole9)((0, 9))
  }

  test("isEmpty") {
    fromFoldable.isEmpty(indexedList) shouldEqual false
    fromFoldable.isEmpty(emptyIndexedList) shouldEqual true
    foldable.isEmpty(whole9) shouldEqual false
  }

  test("nonEmpty") {
    fromFoldable.nonEmpty(indexedList) shouldEqual true
    fromFoldable.nonEmpty(emptyIndexedList) shouldEqual false
    fromFoldable.nonEmpty(indexedList) shouldEqual !fromFoldable.isEmpty(indexedList)
    foldable.nonEmpty(whole9) shouldEqual true
    foldable.nonEmpty(whole9) shouldEqual !foldable.isEmpty(whole9)
  }

  test("length") {
    fromFoldable.length(indexedList) shouldEqual indexedList.length
    fromFoldable.length(emptyIndexedList) shouldEqual 0
    foldable.length(whole9) shouldEqual 1
  }

  test("find") {
    fromFoldable.find(greaterThan5 compose Tuple2._2)(indexedList) shouldEqual list.find(greaterThan5)
    fromFoldable.find(greaterThan10 compose Tuple2._2)(indexedList) shouldEqual None
    foldable.find(greaterThan5 compose Tuple2._2)(whole9) shouldEqual 9.some
    foldable.find(greaterThan10 compose Tuple2._2)(whole9) shouldEqual None
  }

  test("first") {
    fromFoldable.first(indexedList) shouldEqual (0, list.head).some
    fromFoldable.first(emptyIndexedList) shouldEqual None
    foldable.first(whole9) shouldEqual (0, 9).some
  }

  test("last") {
    fromFoldable.last(indexedList) shouldEqual (list.length - 1, list.last).some
    fromFoldable.last(emptyIndexedList) shouldEqual None
    foldable.last(whole9) shouldEqual (0, 9).some
  }

  {
    val ev = catsKernelStdOrderForInt

    test("minimum") {
      fromFoldable.minimum(Random.shuffle(indexedList))(ev) shouldEqual list.head.some
      fromFoldable.minimum(emptyIndexedList)(ev) shouldEqual None
      foldable.minimum(whole9)(ev) shouldEqual 9.some
    }

    test("maximum") {
      fromFoldable.maximum(Random.shuffle(indexedList))(ev) shouldEqual list.last.some
      fromFoldable.maximum(emptyIndexedList)(ev) shouldEqual None
      foldable.maximum(whole9)(ev) shouldEqual 9.some
    }
  }

  test("toArray") {
    fromFoldable.toArray(indexedList) shouldEqual indexedList.toArray
    foldable.toArray(whole9) shouldEqual Array((0, 9))
  }

  test("toList") {
    fromFoldable.toList(indexedList) shouldEqual indexedList
    foldable.toList(whole9) shouldEqual List((0, 9))
  }

  test("use") {
    implicit val state: State[List[(Int, Int)], Int] = State.pure[List[(Int, Int)], Int](1)

    fromFoldable.use.runA(indexedList).value shouldEqual indexedList
    foldable.use.runA(whole9).value shouldEqual List((0, 9))
  }

  test("replicate") {
    replicated.viewAll(1) shouldEqual ones
    replicated.foldMap(1) { case (i, a) => if (i % 2 === 0) a else 0 } shouldEqual
      ones.filter(_._1 % 2 == 0).map(_._2).sum
  }

  test("unfold") {
    unfolded.viewAll(foldState) shouldEqual List.tabulate(10)(i => (0, (i, i + 1)))
    unfolded.length(foldState) shouldEqual 10
  }

  test("filtered") {
    val composed = unfolded compose filtered
    composed.foldMap(foldState)(_._2) shouldEqual 30
    composed.foldMap(foldState)(_._1) shouldEqual 25
  }
}
