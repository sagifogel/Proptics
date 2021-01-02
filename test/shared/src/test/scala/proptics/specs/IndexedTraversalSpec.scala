package proptics.specs

import scala.Function.const

import cats.data.NonEmptyList
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import spire.std.boolean._

import proptics.IndexedTraversal
import proptics.instances.traversalWithIndex._
import proptics.law.discipline._
import proptics.specs.compose._
import proptics.syntax.indexedTraversal._
import proptics.syntax.tuple._

class IndexedTraversalSpec extends PropticsSuite {
  val indexedNel: NonEmptyList[(Int, Int)] = nel.zipWithIndex
  val nelTail: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(nel.tail)
  val singletonNel: NonEmptyList[Int] = NonEmptyList.one(nel.head)
  val idxNelTail: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(nel.tail)
  val idxSingletonNel: NonEmptyList[Int] = NonEmptyList.one(nel.head)
  val nelBool: NonEmptyList[Boolean] = NonEmptyList.fromListUnsafe(boolList)
  val nelFalseBool: NonEmptyList[Boolean] = NonEmptyList.fromListUnsafe(falseBoolList)

  val wholeTraversal: IndexedTraversal[Int, Whole, Int] =
    IndexedTraversal[Int, Whole, Int](whole => (whole.part, 0))(whole => part => whole.copy(part = part))

  val nelIndexedTraversal: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal[Int, NonEmptyList[Int], Int](nel => (nel.head, 0))(nel => i => nel.copy(head = i))

  val fromTraversal: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal.fromTraverse[NonEmptyList, Int, Int]

  val listFromTraversal: IndexedTraversal[Int, List[Int], Int] =
    IndexedTraversal.fromTraverse[List, Int, Int]

  val boolIndexedTraversal: IndexedTraversal[Int, NonEmptyList[Boolean], Boolean] =
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
    fromTraversal.viewAll(nel) shouldEqual indexedNel.toList
    fromIndexableTraverse.viewAll(nel) shouldEqual indexedNel.toList
    nelIndexedTraversal.viewAll(nel) shouldEqual List((1, 0))
  }

  test("preview") {
    fromTraversal.preview(nel) shouldEqual Some((1, 0))
    fromIndexableTraverse.preview(nel) shouldEqual Some((1, 0))
    nelIndexedTraversal.preview(nel) shouldEqual Some((1, 0))
  }

  test("set") {
    fromTraversal.set(0)(nel) shouldEqual nel.map(const(0))
    fromIndexableTraverse.set(0)(nel) shouldEqual nel.map(const(0))
    nelIndexedTraversal.set(0)(nel) shouldEqual nel.copy(head = 0)
  }

  test("over") {
    fromTraversal.over(_._1 + 1)(nel) shouldEqual nel.map(_ + 1)
    fromIndexableTraverse.over(_._1 + 1)(nel) shouldEqual nel.map(_ + 1)
    nelIndexedTraversal.over(oneToNine)(nel) shouldEqual nel.copy(head = 9)
  }

  test("traverse") {
    fromTraversal.traverse(nel)(_._1.some) shouldEqual nel.some
    fromTraversal.traverse(nel)(const(none[Int])) shouldEqual None
    fromTraversal.traverse(nel)(_._1.some) shouldEqual fromTraversal.overF(_._1.some)(nel)
    fromIndexableTraverse.traverse(nel)(_._1.some) shouldEqual nel.some
    fromIndexableTraverse.traverse(nel)(const(none[Int])) shouldEqual None
    fromIndexableTraverse.traverse(nel)(_._1.some) shouldEqual fromTraversal.overF(_._1.some)(nel)
    nelIndexedTraversal.traverse(nel)(_._1.some) shouldEqual nel.some
  }

  test("foldMap") {
    fromTraversal.foldMap(nel)(_._1.toString) shouldEqual nel.map(_.toString).intercalate("")
    fromIndexableTraverse.foldMap(nel)(_._1.toString) shouldEqual nel.map(_.toString).intercalate("")
    nelIndexedTraversal.foldMap(nel)(_._1.toString) shouldEqual nel.head.toString
  }

  test("foldRight") {
    listFromTraversal.foldRight(list)(List.empty[(Int, Int)])(_ :: _) shouldEqual indexedList
  }

  test("foldLeft") {
    listFromTraversal.foldLeft(list)(List.empty[(Int, Int)])((ls, a) => a :: ls) shouldEqual indexedList.reverse
  }

  test("sequence_") {
    fromTraversal.sequence_[Option](nel) shouldEqual ().some
    fromIndexableTraverse.sequence_[Option](nel) shouldEqual ().some
    nelIndexedTraversal.sequence_[Option](nel) shouldEqual ().some
  }

  test("traverse_") {
    fromTraversal.traverse_(nel)(_._2.some) shouldEqual Some(())
    fromTraversal.traverse_(nel)(const(None)) shouldEqual None
    fromIndexableTraverse.traverse_(nel)(_._2.some) shouldEqual Some(())
    fromIndexableTraverse.traverse_(nel)(const(None)) shouldEqual None
    nelIndexedTraversal.traverse_(nel)(_._2.some) shouldEqual Some(())
    nelIndexedTraversal.traverse_(nel)(const(None)) shouldEqual None
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromTraversal.sum(nel) shouldEqual list.sum
      fromIndexableTraverse.sum(nel) shouldEqual list.sum
      nelIndexedTraversal.sum(nel) shouldEqual list.head
    }

    test("product") {
      fromTraversal.product(nel) shouldEqual list.product
      fromIndexableTraverse.product(nel) shouldEqual list.product
      nelIndexedTraversal.product(nel) shouldEqual list.head
    }
  }

  test("forall") {
    fromTraversal.forall(_._1 < 10)(nel) shouldEqual true
    fromTraversal.forall(_._1 > 10)(nel) shouldEqual false
    fromIndexableTraverse.forall(_._1 < 10)(nel) shouldEqual true
    fromIndexableTraverse.forall(_._1 > 10)(nel) shouldEqual false
    nelIndexedTraversal.forall(_._1 < 10)(nel) shouldEqual true
    nelIndexedTraversal.forall(_._1 > 10)(nel) shouldEqual false
  }

  test("forall using heyting") {
    fromTraversal.forall(nel)(_._1 < 10) shouldEqual true
    fromTraversal.forall(nel)(_._1 > 10) shouldEqual false
    fromIndexableTraverse.forall(nel)(_._1 < 10) shouldEqual true
    fromIndexableTraverse.forall(nel)(_._1 > 10) shouldEqual false
    nelIndexedTraversal.forall(nel)(_._1 < 10) shouldEqual true
    nelIndexedTraversal.forall(nel)(_._1 > 10) shouldEqual false
  }

  test("and") {
    boolIndexedTraversal.and(nelBool) shouldEqual false
    boolIndexedTraversal.and(boolIndexedTraversal.set(true)(nelBool)) shouldEqual true
    boolIndexedTraversal.and(nelFalseBool) shouldEqual false
  }

  test("or") {
    boolIndexedTraversal.or(nelBool) shouldEqual true
    boolIndexedTraversal.or(nelFalseBool) shouldEqual false
  }

  test("any") {
    fromTraversal.any(nel)(greaterThan5 compose Tuple2._1) shouldEqual true
    fromIndexableTraverse.any(nel)(greaterThan5 compose Tuple2._1) shouldEqual true
    nelIndexedTraversal.any(nel)(greaterThan5 compose Tuple2._1) shouldEqual false
  }

  test("exist") {
    fromTraversal.exists(greaterThan5 compose Tuple2._1)(nel) shouldEqual true
    fromTraversal.exists(greaterThan10 compose Tuple2._1)(nel) shouldEqual false
    fromIndexableTraverse.exists(greaterThan5 compose Tuple2._1)(nel) shouldEqual true
    fromIndexableTraverse.exists(greaterThan10 compose Tuple2._1)(nel) shouldEqual false
    nelIndexedTraversal.exists(greaterThan5 compose Tuple2._1)(nel) shouldEqual false
    nelIndexedTraversal.exists(greaterThan5 compose Tuple2._1)(nel.copy(head = 10)) shouldEqual true
    nelIndexedTraversal.exists(greaterThan10 compose Tuple2._1)(nel.copy(head = 20)) shouldEqual true
  }

  test("notExists") {
    fromTraversal.notExists(greaterThan5 compose Tuple2._1)(nel) shouldEqual false
    fromTraversal.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual
      !fromTraversal.exists(greaterThan10 compose Tuple2._1)(nel)
    fromIndexableTraverse.notExists(greaterThan5 compose Tuple2._1)(nel) shouldEqual false
    fromIndexableTraverse.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual true
    fromIndexableTraverse.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual
      !fromIndexableTraverse.exists(greaterThan10 compose Tuple2._1)(nel)
    nelIndexedTraversal.notExists(greaterThan5 compose Tuple2._1)(nel) shouldEqual true
    nelIndexedTraversal.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual true
    nelIndexedTraversal.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual
      !nelIndexedTraversal.exists(greaterThan10 compose Tuple2._1)(nel)
  }

  test("contains") {
    fromTraversal.contains((1, 0))(nel) shouldEqual true
    fromTraversal.contains((5, 4))(nel) shouldEqual true
    fromTraversal.contains((10, 0))(nel) shouldEqual false
    fromTraversal.contains((5, 1))(nel) shouldEqual false
    fromIndexableTraverse.contains((1, 0))(nel) shouldEqual true
    fromIndexableTraverse.contains((5, 4))(nel) shouldEqual true
    fromIndexableTraverse.contains((10, 0))(nel) shouldEqual false
    fromIndexableTraverse.contains((5, 1))(nel) shouldEqual false
    nelIndexedTraversal.contains((1, 0))(nel) shouldEqual true
    nelIndexedTraversal.contains((10, 0))(nel) shouldEqual false
    nelIndexedTraversal.contains((9, 1))(nel) shouldEqual false
  }

  test("notContains") {
    fromTraversal.notContains((1, 0))(nel) shouldEqual false
    fromTraversal.notContains((5, 4))(nel) shouldEqual false
    fromTraversal.notContains((10, 0))(nel) shouldEqual true
    fromTraversal.notContains((5, 1))(nel) shouldEqual true
    fromTraversal.notContains((10, 0))(nel) shouldEqual !fromTraversal.contains((0, 10))(nel)
    fromIndexableTraverse.notContains((1, 0))(nel) shouldEqual false
    fromIndexableTraverse.notContains((5, 4))(nel) shouldEqual false
    fromIndexableTraverse.notContains((10, 0))(nel) shouldEqual true
    fromIndexableTraverse.notContains((5, 1))(nel) shouldEqual true
    fromIndexableTraverse.notContains((10, 0))(nel) shouldEqual !fromIndexableTraverse.contains((10, 0))(nel)
    nelIndexedTraversal.notContains((1, 0))(nel) shouldEqual false
    nelIndexedTraversal.notContains((10, 0))(nel) shouldEqual true
    nelIndexedTraversal.notContains((5, 1))(nel) shouldEqual true
    nelIndexedTraversal.notContains((10, 0))(nel) shouldEqual !nelIndexedTraversal.contains((10, 0))(nel)
  }

  test("isEmpty") {
    fromTraversal.isEmpty(nel) shouldEqual false
    fromIndexableTraverse.isEmpty(nel) shouldEqual false
    nelIndexedTraversal.isEmpty(nel) shouldEqual false
  }

  test("nonEmpty") {
    fromTraversal.nonEmpty(nel) shouldEqual true
    fromTraversal.nonEmpty(nel) shouldEqual !fromTraversal.isEmpty(nel)
    fromIndexableTraverse.nonEmpty(nel) shouldEqual true
    fromIndexableTraverse.nonEmpty(nel) shouldEqual !fromTraversal.isEmpty(nel)
    nelIndexedTraversal.nonEmpty(nel) shouldEqual true
    nelIndexedTraversal.nonEmpty(nel) shouldEqual !nelIndexedTraversal.isEmpty(nel)
  }

  test("length") {
    fromTraversal.length(nel) shouldEqual indexedNel.length
    fromIndexableTraverse.length(nel) shouldEqual indexedNel.length
    nelIndexedTraversal.length(nel) shouldEqual 1
  }

  test("find") {
    fromTraversal.find(greaterThan5 compose Tuple2._1)(nel) shouldEqual list.find(greaterThan5)
    fromTraversal.find(greaterThan10 compose Tuple2._1)(nel) shouldEqual None
    fromIndexableTraverse.find(greaterThan5 compose Tuple2._1)(nel) shouldEqual list.find(greaterThan5)
    fromIndexableTraverse.find(greaterThan10 compose Tuple2._1)(nel) shouldEqual None
    nelIndexedTraversal.find(_._1 >= 1)(nel) shouldEqual 1.some
    nelIndexedTraversal.find(greaterThan5 compose Tuple2._1)(nel) shouldEqual None
    nelIndexedTraversal.find(greaterThan10 compose Tuple2._1)(nel) shouldEqual None
  }

  test("first") {
    fromTraversal.first(nel) shouldEqual (1, 0).some
    fromIndexableTraverse.first(nel) shouldEqual (1, 0).some
    nelIndexedTraversal.first(nel) shouldEqual (1, 0).some
  }

  test("last") {
    fromTraversal.last(nel) shouldEqual (6, 5).some
    fromIndexableTraverse.last(nel) shouldEqual (6, 5).some
    nelIndexedTraversal.last(nel) shouldEqual (1, 0).some
  }

  test("minimum") {
    fromTraversal.minimum(nel) shouldEqual indexedNel.head._1.some
    fromIndexableTraverse.minimum(nel) shouldEqual indexedNel.head._1.some
    nelIndexedTraversal.minimum(nel) shouldEqual nel.head.some
  }

  test("maximum") {
    fromTraversal.maximum(nel) shouldEqual indexedNel.last._1.some
    fromIndexableTraverse.maximum(nel) shouldEqual indexedNel.last._1.some
    nelIndexedTraversal.maximum(nel) shouldEqual nel.head.some
  }

  test("toArray") {
    fromTraversal.toArray(nel) shouldEqual nel.toList.toArray
    fromIndexableTraverse.toArray(nel) shouldEqual nel.toList.toArray
    nelIndexedTraversal.toArray(nel) shouldEqual Array(nel.head)
  }

  test("toList") {
    fromTraversal.toList(nel) shouldEqual nel.toList
    fromIndexableTraverse.toList(nel) shouldEqual nel.toList
    nelIndexedTraversal.toList(nel) shouldEqual List(nel.head)
  }

  test("use") {
    fromTraversal.use.runA(nel).value shouldEqual indexedNel.toList
    fromIndexableTraverse.use.runA(nel).value shouldEqual indexedNel.toList
    nelIndexedTraversal.use.runA(nel).value shouldEqual List((1, 0))
  }

  test("reindex") {
    fromTraversal
      .reindex(_.toString)
      .viewAll(nel) shouldEqual indexedList.map(_.map(_.toString))
  }

  test("compose with IndexedGetter") {
    (indexedTraversal compose indexedGetter).foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    (indexedTraversal compose indexedFold).foldMap(9)(_._1) shouldEqual 9
  }

  test("filterByIndex") {
    listFromTraversal.filterByIndex(_ < 3).viewAll(list) shouldEqual indexedList.take(3)
  }

  test("element") {
    listFromTraversal.element(1).preview(list) shouldEqual 2.some
  }
}
