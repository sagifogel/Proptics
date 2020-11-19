package proptics.specs

import scala.Function.const

import cats.data.NonEmptyList
import cats.syntax.foldable._
import cats.syntax.option._
import spire.std.boolean._

import proptics.law.discipline._
import proptics.specs.compose._
import proptics.syntax.indexedTraversal._
import proptics.syntax.tuple._
import proptics.{IndexedTraversal, IndexedTraversal_}

class IndexedTraversalSpec extends PropticsSuite {
  val listWithIdx: List[(Int, Int)] = list.zipWithIndex.map(_.swap)
  val indexedNel: NonEmptyList[(Int, Int)] = nel.zipWithIndex.map(_.swap)
  val nelTail: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(nel.tail)
  val singletonNel: NonEmptyList[Int] = NonEmptyList.one(nel.head)
  val idxNelTail: NonEmptyList[(Int, Int)] = NonEmptyList.fromListUnsafe(indexedNel.tail)
  val idxSingletonNel: NonEmptyList[(Int, Int)] = NonEmptyList.one(indexedNel.head)
  val nelBool: NonEmptyList[(Int, Boolean)] = NonEmptyList.fromListUnsafe(boolList).zipWithIndex.map(_.swap)
  val nelFalseBool: NonEmptyList[(Int, Boolean)] = NonEmptyList.fromListUnsafe(falseBoolList).zipWithIndex.map(_.swap)

  val wholeTraversal: IndexedTraversal[Int, Whole, Int] =
    IndexedTraversal[Int, Whole, Int](whole => (0, whole.part))(whole => part => whole.copy(part = part))

  val nelIndexedTraversal: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal[Int, NonEmptyList[Int], Int](nel => (0, nel.head))(nel => i => nel.copy(head = i))

  val fromTraversal: IndexedTraversal_[Int, NonEmptyList[(Int, Int)], NonEmptyList[Int], Int, Int] =
    IndexedTraversal.fromTraverse[NonEmptyList, Int, Int]

  val listFromTraversal: IndexedTraversal_[Int, List[(Int, Int)], List[Int], Int, Int] =
    IndexedTraversal.fromTraverse[List, Int, Int]

  val boolIndexedTraversal: IndexedTraversal_[Int, NonEmptyList[(Int, Boolean)], NonEmptyList[Boolean], Boolean, Boolean] =
    IndexedTraversal.fromTraverse[NonEmptyList, Int, Boolean]

  val fromIndexableTraverse: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal.fromIndexableTraverse[NonEmptyList, Int]

  checkAll("IndexedTraversal[Int, NonEmptyList[Int], Int] apply", IndexedTraversalTests(nelIndexedTraversal).indexedTraversal)
  checkAll("fromIndexableTraverse IndexedTraversal[Int, NonEmptyList[Int], Int] apply", IndexedTraversalTests(fromIndexableTraverse).indexedTraversal)
  checkAll("IndexedTraversal[Int, Whole, Int] asTraversal", TraversalTests(wholeTraversal.asTraversal).traversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedLens[Int, Int]", IndexedTraversalTests(indexedTraversal compose indexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with AnIndexedLens[Int, Int]", IndexedTraversalTests(indexedTraversal compose anIndexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedTraversal[Int, Int]", IndexedTraversalTests(indexedTraversal compose indexedTraversal).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedSetter[Int, Int]", IndexedSetterTests(indexedTraversal compose indexedSetter).indexedSetter)

  test("viewAll") {
    fromTraversal.viewAll(indexedNel) shouldEqual indexedNel.toList
    fromIndexableTraverse.viewAll(nel) shouldEqual indexedNel.toList
    nelIndexedTraversal.viewAll(nel) shouldEqual List((0, 1))
  }

  test("preview") {
    fromTraversal.preview(indexedNel) shouldEqual Some((0, 1))
    fromIndexableTraverse.preview(nel) shouldEqual Some((0, 1))
    nelIndexedTraversal.preview(nel) shouldEqual Some((0, 1))
  }

  test("set") {
    fromTraversal.set(0)(indexedNel) shouldEqual nel.map(const(0))
    fromIndexableTraverse.set(0)(nel) shouldEqual nel.map(const(0))
    nelIndexedTraversal.set(0)(nel) shouldEqual nel.copy(head = 0)
  }

  test("over") {
    fromTraversal.over(_._2 + 1)(indexedNel) shouldEqual nel.map(_ + 1)
    fromIndexableTraverse.over(_._2 + 1)(nel) shouldEqual nel.map(_ + 1)
    nelIndexedTraversal.over(oneToNine)(nel) shouldEqual nel.copy(head = 9)
  }

  test("traverse") {
    fromTraversal.traverse(indexedNel)(_._2.some) shouldEqual nel.some
    fromTraversal.traverse(indexedNel)(const(none[Int])) shouldEqual None
    fromTraversal.traverse(indexedNel)(_._2.some) shouldEqual fromTraversal.overF(_._2.some)(indexedNel)
    fromIndexableTraverse.traverse(nel)(_._2.some) shouldEqual nel.some
    fromIndexableTraverse.traverse(nel)(const(none[Int])) shouldEqual None
    fromIndexableTraverse.traverse(nel)(_._2.some) shouldEqual fromTraversal.overF(_._2.some)(indexedNel)
    nelIndexedTraversal.traverse(nel)(_._2.some) shouldEqual nel.some
  }

  test("foldMap") {
    fromTraversal.foldMap(indexedNel)(_._2.toString) shouldEqual nel.map(_.toString).intercalate("")
    fromIndexableTraverse.foldMap(nel)(_._2.toString) shouldEqual nel.map(_.toString).intercalate("")
    nelIndexedTraversal.foldMap(nel)(_._2.toString) shouldEqual nel.head.toString
  }

  test("foldr") {
    listFromTraversal.foldr(listWithIdx)(List.empty[(Int, Int)])(_ :: _) shouldEqual listWithIdx
  }

  test("foldl") {
    listFromTraversal.foldl(listWithIdx)(List.empty[(Int, Int)])((ls, a) => a :: ls) shouldEqual listWithIdx.reverse
  }

  test("sequence_") {
    fromTraversal.sequence_[Option](indexedNel) shouldEqual ().some
    fromIndexableTraverse.sequence_[Option](nel) shouldEqual ().some
    nelIndexedTraversal.sequence_[Option](nel) shouldEqual ().some
  }

  test("traverse_") {
    fromTraversal.traverse_(indexedNel)(_._2.some) shouldEqual Some(())
    fromTraversal.traverse_(indexedNel)(const(None)) shouldEqual None
    fromIndexableTraverse.traverse_(nel)(_._2.some) shouldEqual Some(())
    fromIndexableTraverse.traverse_(nel)(const(None)) shouldEqual None
    nelIndexedTraversal.traverse_(nel)(_._2.some) shouldEqual Some(())
    nelIndexedTraversal.traverse_(nel)(const(None)) shouldEqual None
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromTraversal.sum(indexedNel) shouldEqual list.sum
      fromIndexableTraverse.sum(nel) shouldEqual list.sum
      nelIndexedTraversal.sum(nel) shouldEqual list.head
    }

    test("product") {
      fromTraversal.product(indexedNel) shouldEqual list.product
      fromIndexableTraverse.product(nel) shouldEqual list.product
      nelIndexedTraversal.product(nel) shouldEqual list.head
    }
  }

  test("forall") {
    fromTraversal.forall(_._2 < 10)(indexedNel) shouldEqual true
    fromTraversal.forall(_._2 > 10)(indexedNel) shouldEqual false
    fromIndexableTraverse.forall(_._2 < 10)(nel) shouldEqual true
    fromIndexableTraverse.forall(_._2 > 10)(nel) shouldEqual false
    nelIndexedTraversal.forall(_._2 < 10)(nel) shouldEqual true
    nelIndexedTraversal.forall(_._2 > 10)(nel) shouldEqual false
  }

  test("forall using heyting") {
    fromTraversal.forall(indexedNel)(_._2 < 10) shouldEqual true
    fromTraversal.forall(indexedNel)(_._2 > 10) shouldEqual false
    fromIndexableTraverse.forall(nel)(_._2 < 10) shouldEqual true
    fromIndexableTraverse.forall(nel)(_._2 > 10) shouldEqual false
    nelIndexedTraversal.forall(nel)(_._2 < 10) shouldEqual true
    nelIndexedTraversal.forall(nel)(_._2 > 10) shouldEqual false
  }

  test("and") {
    boolIndexedTraversal.and(nelBool) shouldEqual false
    boolIndexedTraversal.and(boolIndexedTraversal.set(true)(nelBool).zipWithIndex.map(_.swap)) shouldEqual true
    boolIndexedTraversal.and(nelFalseBool) shouldEqual false
  }

  test("or") {
    boolIndexedTraversal.or(nelBool) shouldEqual true
    boolIndexedTraversal.or(nelFalseBool) shouldEqual false
  }

  test("any") {
    fromTraversal.any(indexedNel)(greaterThan5 compose Tuple2._2) shouldEqual true
    fromIndexableTraverse.any(nel)(greaterThan5 compose Tuple2._2) shouldEqual true
    nelIndexedTraversal.any(nel)(greaterThan5 compose Tuple2._2) shouldEqual false
  }

  test("exist") {
    fromTraversal.exists(greaterThan5 compose Tuple2._2)(indexedNel) shouldEqual true
    fromTraversal.exists(greaterThan10 compose Tuple2._2)(indexedNel) shouldEqual false
    fromIndexableTraverse.exists(greaterThan5 compose Tuple2._2)(nel) shouldEqual true
    fromIndexableTraverse.exists(greaterThan10 compose Tuple2._2)(nel) shouldEqual false
    nelIndexedTraversal.exists(greaterThan5 compose Tuple2._2)(nel) shouldEqual false
    nelIndexedTraversal.exists(greaterThan5 compose Tuple2._2)(nel.copy(head = 10)) shouldEqual true
    nelIndexedTraversal.exists(greaterThan10 compose Tuple2._2)(nel.copy(head = 20)) shouldEqual true
  }

  test("notExists") {
    fromTraversal.notExists(greaterThan5 compose Tuple2._2)(indexedNel) shouldEqual false
    fromTraversal.notExists(greaterThan10 compose Tuple2._2)(indexedNel) shouldEqual true
    fromTraversal.notExists(greaterThan10 compose Tuple2._2)(indexedNel) shouldEqual
      !fromTraversal.exists(greaterThan10 compose Tuple2._2)(indexedNel)
    fromIndexableTraverse.notExists(greaterThan5 compose Tuple2._2)(nel) shouldEqual false
    fromIndexableTraverse.notExists(greaterThan10 compose Tuple2._2)(nel) shouldEqual true
    fromIndexableTraverse.notExists(greaterThan10 compose Tuple2._2)(nel) shouldEqual
      !fromIndexableTraverse.exists(greaterThan10 compose Tuple2._2)(nel)
    nelIndexedTraversal.notExists(greaterThan5 compose Tuple2._2)(nel) shouldEqual true
    nelIndexedTraversal.notExists(greaterThan10 compose Tuple2._2)(nel) shouldEqual true
    nelIndexedTraversal.notExists(greaterThan10 compose Tuple2._2)(nel) shouldEqual
      !nelIndexedTraversal.exists(greaterThan10 compose Tuple2._2)(nel)
  }

  test("contains") {
    fromTraversal.contains((0, 1))(indexedNel) shouldEqual true
    fromTraversal.contains((4, 5))(indexedNel) shouldEqual true
    fromTraversal.contains((0, 10))(indexedNel) shouldEqual false
    fromTraversal.contains((1, 5))(indexedNel) shouldEqual false
    fromIndexableTraverse.contains((0, 1))(nel) shouldEqual true
    fromIndexableTraverse.contains((4, 5))(nel) shouldEqual true
    fromIndexableTraverse.contains((0, 10))(nel) shouldEqual false
    fromIndexableTraverse.contains((1, 5))(nel) shouldEqual false
    nelIndexedTraversal.contains((0, 1))(nel) shouldEqual true
    nelIndexedTraversal.contains((0, 10))(nel) shouldEqual false
    nelIndexedTraversal.contains((1, 9))(nel) shouldEqual false
  }

  test("notContains") {
    fromTraversal.notContains((0, 1))(indexedNel) shouldEqual false
    fromTraversal.notContains((4, 5))(indexedNel) shouldEqual false
    fromTraversal.notContains((0, 10))(indexedNel) shouldEqual true
    fromTraversal.notContains((1, 5))(indexedNel) shouldEqual true
    fromTraversal.notContains((0, 10))(indexedNel) shouldEqual !fromTraversal.contains((0, 10))(indexedNel)
    fromIndexableTraverse.notContains((0, 1))(nel) shouldEqual false
    fromIndexableTraverse.notContains((4, 5))(nel) shouldEqual false
    fromIndexableTraverse.notContains((0, 10))(nel) shouldEqual true
    fromIndexableTraverse.notContains((1, 5))(nel) shouldEqual true
    fromIndexableTraverse.notContains((0, 10))(nel) shouldEqual !fromIndexableTraverse.contains((0, 10))(nel)
    nelIndexedTraversal.notContains((0, 1))(nel) shouldEqual false
    nelIndexedTraversal.notContains((0, 10))(nel) shouldEqual true
    nelIndexedTraversal.notContains((1, 5))(nel) shouldEqual true
    nelIndexedTraversal.notContains((0, 10))(nel) shouldEqual !nelIndexedTraversal.contains((0, 10))(nel)
  }

  test("isEmpty") {
    fromTraversal.isEmpty(indexedNel) shouldEqual false
    fromIndexableTraverse.isEmpty(nel) shouldEqual false
    nelIndexedTraversal.isEmpty(nel) shouldEqual false
  }

  test("nonEmpty") {
    fromTraversal.nonEmpty(indexedNel) shouldEqual true
    fromTraversal.nonEmpty(indexedNel) shouldEqual !fromTraversal.isEmpty(indexedNel)
    fromIndexableTraverse.nonEmpty(nel) shouldEqual true
    fromIndexableTraverse.nonEmpty(nel) shouldEqual !fromTraversal.isEmpty(indexedNel)
    nelIndexedTraversal.nonEmpty(nel) shouldEqual true
    nelIndexedTraversal.nonEmpty(nel) shouldEqual !nelIndexedTraversal.isEmpty(nel)
  }

  test("length") {
    fromTraversal.length(indexedNel) shouldEqual indexedNel.length
    fromIndexableTraverse.length(nel) shouldEqual indexedNel.length
    nelIndexedTraversal.length(nel) shouldEqual 1
  }

  test("find") {
    fromTraversal.find(greaterThan5 compose Tuple2._2)(indexedNel) shouldEqual list.find(greaterThan5)
    fromTraversal.find(greaterThan10 compose Tuple2._2)(indexedNel) shouldEqual None
    fromIndexableTraverse.find(greaterThan5 compose Tuple2._2)(nel) shouldEqual list.find(greaterThan5)
    fromIndexableTraverse.find(greaterThan10 compose Tuple2._2)(nel) shouldEqual None
    nelIndexedTraversal.find(_._2 >= 1)(nel) shouldEqual 1.some
    nelIndexedTraversal.find(greaterThan5 compose Tuple2._2)(nel) shouldEqual None
    nelIndexedTraversal.find(greaterThan10 compose Tuple2._2)(nel) shouldEqual None
  }

  test("first") {
    fromTraversal.first(indexedNel) shouldEqual (0, 1).some
    fromIndexableTraverse.first(nel) shouldEqual (0, 1).some
    nelIndexedTraversal.first(nel) shouldEqual (0, 1).some
  }

  test("last") {
    fromTraversal.last(indexedNel) shouldEqual (5, 6).some
    fromIndexableTraverse.last(nel) shouldEqual (5, 6).some
    nelIndexedTraversal.last(nel) shouldEqual (0, 1).some
  }

  test("minimum") {
    fromTraversal.minimum(indexedNel) shouldEqual indexedNel.head._2.some
    fromIndexableTraverse.minimum(nel) shouldEqual indexedNel.head._2.some
    nelIndexedTraversal.minimum(nel) shouldEqual nel.head.some
  }

  test("maximum") {
    fromTraversal.maximum(indexedNel) shouldEqual indexedNel.last._2.some
    fromIndexableTraverse.maximum(nel) shouldEqual indexedNel.last._2.some
    nelIndexedTraversal.maximum(nel) shouldEqual nel.head.some
  }

  test("toArray") {
    fromTraversal.toArray(indexedNel) shouldEqual indexedNel.toList.toArray
    fromIndexableTraverse.toArray(nel) shouldEqual indexedNel.toList.toArray
    nelIndexedTraversal.toArray(nel) shouldEqual Array((0, nel.head))
  }

  test("toList") {
    fromTraversal.toList(indexedNel) shouldEqual indexedNel.toList
    fromIndexableTraverse.toList(nel) shouldEqual indexedNel.toList
    nelIndexedTraversal.toList(nel) shouldEqual List((0, nel.head))
  }

  test("use") {
    fromTraversal.use.runA(indexedNel).value shouldEqual indexedNel.toList
    fromIndexableTraverse.use.runA(nel).value shouldEqual indexedNel.toList
    nelIndexedTraversal.use.runA(nel).value shouldEqual List((0, 1))
  }

  test("compose with IndexedGetter") {
    (indexedTraversal compose indexedGetter).foldMap(9)(_._2) shouldEqual 9
  }

  test("compose with IndexedFold") {
    (indexedTraversal compose indexedFold).foldMap(9)(_._2) shouldEqual 9
  }

  test("filterByIndex") {
    listFromTraversal.filterByIndex(_ < 3).viewAll(listWithIdx) shouldEqual listWithIdx.take(3)
  }

  test("element") {
    listFromTraversal.element(1).viewAll(listWithIdx) shouldEqual List((1, 2))
  }
}
