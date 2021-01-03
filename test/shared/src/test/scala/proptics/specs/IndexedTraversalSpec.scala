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

  val nelFromTraversalWithIndex: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal.fromTraverseWithIndex[NonEmptyList, Int, Int]

  val listFromTraversalWithIndex: IndexedTraversal[Int, List[Int], Int] =
    IndexedTraversal.fromTraverseWithIndex[List, Int, Int]

  val boolIndexedTraversalWithIndex: IndexedTraversal[Int, NonEmptyList[Boolean], Boolean] =
    IndexedTraversal.fromTraverseWithIndex[NonEmptyList, Int, Boolean]

  val fromTraverse: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal.fromTraverse[NonEmptyList, Int]

  checkAll("IndexedTraversal[Int, Whole, Int] asTraversal", TraversalTests(wholeTraversal.asTraversal).traversal)
  checkAll("IndexedTraversal[Int, NonEmptyList[Int], Int] apply", IndexedTraversalTests(nelIndexedTraversal).indexedTraversal)
  checkAll("IndexedTraversal[Int, NonEmptyList[Int], Int] fromTraverse", IndexedTraversalTests(fromTraverse).indexedTraversal)
  checkAll("IndexedTraversal[Int, List[Int], Int] fromTraverseWithIndex", IndexedTraversalTests(listFromTraversalWithIndex).indexedTraversal)
  checkAll("IndexedTraversal[Int, NonEmptyList[Int], Int] fromTraverseWithIndex", IndexedTraversalTests(nelFromTraversalWithIndex).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedLens[Int, Int]", IndexedTraversalTests(indexedTraversal compose indexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with AnIndexedLens[Int, Int]", IndexedTraversalTests(indexedTraversal compose anIndexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedTraversal[Int, Int]", IndexedTraversalTests(indexedTraversal compose indexedTraversal).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedSetter[Int, Int]", IndexedSetterTests(indexedTraversal compose indexedSetter).indexedSetter)

  test("viewAll") {
    nelFromTraversalWithIndex.viewAll(nel) shouldEqual indexedNel.toList
    fromTraverse.viewAll(nel) shouldEqual indexedNel.toList
    nelIndexedTraversal.viewAll(nel) shouldEqual List((1, 0))
  }

  test("preview") {
    nelFromTraversalWithIndex.preview(nel) shouldEqual Some((1, 0))
    fromTraverse.preview(nel) shouldEqual Some((1, 0))
    nelIndexedTraversal.preview(nel) shouldEqual Some((1, 0))
  }

  test("set") {
    nelFromTraversalWithIndex.set(0)(nel) shouldEqual nel.map(const(0))
    fromTraverse.set(0)(nel) shouldEqual nel.map(const(0))
    nelIndexedTraversal.set(0)(nel) shouldEqual nel.copy(head = 0)
  }

  test("over") {
    nelFromTraversalWithIndex.over(_._1 + 1)(nel) shouldEqual nel.map(_ + 1)
    fromTraverse.over(_._1 + 1)(nel) shouldEqual nel.map(_ + 1)
    nelIndexedTraversal.over(oneToNine)(nel) shouldEqual nel.copy(head = 9)
  }

  test("traverse") {
    nelFromTraversalWithIndex.traverse(nel)(_._1.some) shouldEqual nel.some
    nelFromTraversalWithIndex.traverse(nel)(const(none[Int])) shouldEqual None
    nelFromTraversalWithIndex.traverse(nel)(_._1.some) shouldEqual nelFromTraversalWithIndex.overF(_._1.some)(nel)
    fromTraverse.traverse(nel)(_._1.some) shouldEqual nel.some
    fromTraverse.traverse(nel)(const(none[Int])) shouldEqual None
    fromTraverse.traverse(nel)(_._1.some) shouldEqual nelFromTraversalWithIndex.overF(_._1.some)(nel)
    nelIndexedTraversal.traverse(nel)(_._1.some) shouldEqual nel.some
  }

  test("foldMap") {
    nelFromTraversalWithIndex.foldMap(nel)(_._1.toString) shouldEqual nel.map(_.toString).intercalate("")
    fromTraverse.foldMap(nel)(_._1.toString) shouldEqual nel.map(_.toString).intercalate("")
    nelIndexedTraversal.foldMap(nel)(_._1.toString) shouldEqual nel.head.toString
  }

  test("foldRight") {
    listFromTraversalWithIndex.foldRight(list)(List.empty[(Int, Int)])(_ :: _) shouldEqual indexedList
  }

  test("foldLeft") {
    listFromTraversalWithIndex.foldLeft(list)(List.empty[(Int, Int)])((ls, a) => a :: ls) shouldEqual indexedList.reverse
  }

  test("sequence_") {
    nelFromTraversalWithIndex.sequence_[Option](nel) shouldEqual ().some
    fromTraverse.sequence_[Option](nel) shouldEqual ().some
    nelIndexedTraversal.sequence_[Option](nel) shouldEqual ().some
  }

  test("traverse_") {
    nelFromTraversalWithIndex.traverse_(nel)(_._2.some) shouldEqual Some(())
    nelFromTraversalWithIndex.traverse_(nel)(const(None)) shouldEqual None
    fromTraverse.traverse_(nel)(_._2.some) shouldEqual Some(())
    fromTraverse.traverse_(nel)(const(None)) shouldEqual None
    nelIndexedTraversal.traverse_(nel)(_._2.some) shouldEqual Some(())
    nelIndexedTraversal.traverse_(nel)(const(None)) shouldEqual None
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      nelFromTraversalWithIndex.sum(nel) shouldEqual list.sum
      fromTraverse.sum(nel) shouldEqual list.sum
      nelIndexedTraversal.sum(nel) shouldEqual list.head
    }

    test("product") {
      nelFromTraversalWithIndex.product(nel) shouldEqual list.product
      fromTraverse.product(nel) shouldEqual list.product
      nelIndexedTraversal.product(nel) shouldEqual list.head
    }
  }

  test("forall") {
    nelFromTraversalWithIndex.forall(_._1 < 10)(nel) shouldEqual true
    nelFromTraversalWithIndex.forall(_._1 > 10)(nel) shouldEqual false
    fromTraverse.forall(_._1 < 10)(nel) shouldEqual true
    fromTraverse.forall(_._1 > 10)(nel) shouldEqual false
    nelIndexedTraversal.forall(_._1 < 10)(nel) shouldEqual true
    nelIndexedTraversal.forall(_._1 > 10)(nel) shouldEqual false
  }

  test("forall using heyting") {
    nelFromTraversalWithIndex.forall(nel)(_._1 < 10) shouldEqual true
    nelFromTraversalWithIndex.forall(nel)(_._1 > 10) shouldEqual false
    fromTraverse.forall(nel)(_._1 < 10) shouldEqual true
    fromTraverse.forall(nel)(_._1 > 10) shouldEqual false
    nelIndexedTraversal.forall(nel)(_._1 < 10) shouldEqual true
    nelIndexedTraversal.forall(nel)(_._1 > 10) shouldEqual false
  }

  test("and") {
    boolIndexedTraversalWithIndex.and(nelBool) shouldEqual false
    boolIndexedTraversalWithIndex.and(boolIndexedTraversalWithIndex.set(true)(nelBool)) shouldEqual true
    boolIndexedTraversalWithIndex.and(nelFalseBool) shouldEqual false
  }

  test("or") {
    boolIndexedTraversalWithIndex.or(nelBool) shouldEqual true
    boolIndexedTraversalWithIndex.or(nelFalseBool) shouldEqual false
  }

  test("any") {
    nelFromTraversalWithIndex.any(nel)(greaterThan5 compose Tuple2._1) shouldEqual true
    fromTraverse.any(nel)(greaterThan5 compose Tuple2._1) shouldEqual true
    nelIndexedTraversal.any(nel)(greaterThan5 compose Tuple2._1) shouldEqual false
  }

  test("exist") {
    nelFromTraversalWithIndex.exists(greaterThan5 compose Tuple2._1)(nel) shouldEqual true
    nelFromTraversalWithIndex.exists(greaterThan10 compose Tuple2._1)(nel) shouldEqual false
    fromTraverse.exists(greaterThan5 compose Tuple2._1)(nel) shouldEqual true
    fromTraverse.exists(greaterThan10 compose Tuple2._1)(nel) shouldEqual false
    nelIndexedTraversal.exists(greaterThan5 compose Tuple2._1)(nel) shouldEqual false
    nelIndexedTraversal.exists(greaterThan5 compose Tuple2._1)(nel.copy(head = 10)) shouldEqual true
    nelIndexedTraversal.exists(greaterThan10 compose Tuple2._1)(nel.copy(head = 20)) shouldEqual true
  }

  test("notExists") {
    nelFromTraversalWithIndex.notExists(greaterThan5 compose Tuple2._1)(nel) shouldEqual false
    nelFromTraversalWithIndex.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual
      !nelFromTraversalWithIndex.exists(greaterThan10 compose Tuple2._1)(nel)
    fromTraverse.notExists(greaterThan5 compose Tuple2._1)(nel) shouldEqual false
    fromTraverse.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual true
    fromTraverse.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual
      !fromTraverse.exists(greaterThan10 compose Tuple2._1)(nel)
    nelIndexedTraversal.notExists(greaterThan5 compose Tuple2._1)(nel) shouldEqual true
    nelIndexedTraversal.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual true
    nelIndexedTraversal.notExists(greaterThan10 compose Tuple2._1)(nel) shouldEqual
      !nelIndexedTraversal.exists(greaterThan10 compose Tuple2._1)(nel)
  }

  test("contains") {
    nelFromTraversalWithIndex.contains((1, 0))(nel) shouldEqual true
    nelFromTraversalWithIndex.contains((5, 4))(nel) shouldEqual true
    nelFromTraversalWithIndex.contains((10, 0))(nel) shouldEqual false
    nelFromTraversalWithIndex.contains((5, 1))(nel) shouldEqual false
    fromTraverse.contains((1, 0))(nel) shouldEqual true
    fromTraverse.contains((5, 4))(nel) shouldEqual true
    fromTraverse.contains((10, 0))(nel) shouldEqual false
    fromTraverse.contains((5, 1))(nel) shouldEqual false
    nelIndexedTraversal.contains((1, 0))(nel) shouldEqual true
    nelIndexedTraversal.contains((10, 0))(nel) shouldEqual false
    nelIndexedTraversal.contains((9, 1))(nel) shouldEqual false
  }

  test("notContains") {
    nelFromTraversalWithIndex.notContains((1, 0))(nel) shouldEqual false
    nelFromTraversalWithIndex.notContains((5, 4))(nel) shouldEqual false
    nelFromTraversalWithIndex.notContains((10, 0))(nel) shouldEqual true
    nelFromTraversalWithIndex.notContains((5, 1))(nel) shouldEqual true
    nelFromTraversalWithIndex.notContains((10, 0))(nel) shouldEqual !nelFromTraversalWithIndex.contains((0, 10))(nel)
    fromTraverse.notContains((1, 0))(nel) shouldEqual false
    fromTraverse.notContains((5, 4))(nel) shouldEqual false
    fromTraverse.notContains((10, 0))(nel) shouldEqual true
    fromTraverse.notContains((5, 1))(nel) shouldEqual true
    fromTraverse.notContains((10, 0))(nel) shouldEqual !fromTraverse.contains((10, 0))(nel)
    nelIndexedTraversal.notContains((1, 0))(nel) shouldEqual false
    nelIndexedTraversal.notContains((10, 0))(nel) shouldEqual true
    nelIndexedTraversal.notContains((5, 1))(nel) shouldEqual true
    nelIndexedTraversal.notContains((10, 0))(nel) shouldEqual !nelIndexedTraversal.contains((10, 0))(nel)
  }

  test("isEmpty") {
    nelFromTraversalWithIndex.isEmpty(nel) shouldEqual false
    fromTraverse.isEmpty(nel) shouldEqual false
    nelIndexedTraversal.isEmpty(nel) shouldEqual false
  }

  test("nonEmpty") {
    nelFromTraversalWithIndex.nonEmpty(nel) shouldEqual true
    nelFromTraversalWithIndex.nonEmpty(nel) shouldEqual !nelFromTraversalWithIndex.isEmpty(nel)
    fromTraverse.nonEmpty(nel) shouldEqual true
    fromTraverse.nonEmpty(nel) shouldEqual !nelFromTraversalWithIndex.isEmpty(nel)
    nelIndexedTraversal.nonEmpty(nel) shouldEqual true
    nelIndexedTraversal.nonEmpty(nel) shouldEqual !nelIndexedTraversal.isEmpty(nel)
  }

  test("length") {
    nelFromTraversalWithIndex.length(nel) shouldEqual indexedNel.length
    fromTraverse.length(nel) shouldEqual indexedNel.length
    nelIndexedTraversal.length(nel) shouldEqual 1
  }

  test("find") {
    nelFromTraversalWithIndex.find(greaterThan5 compose Tuple2._1)(nel) shouldEqual list.find(greaterThan5)
    nelFromTraversalWithIndex.find(greaterThan10 compose Tuple2._1)(nel) shouldEqual None
    fromTraverse.find(greaterThan5 compose Tuple2._1)(nel) shouldEqual list.find(greaterThan5)
    fromTraverse.find(greaterThan10 compose Tuple2._1)(nel) shouldEqual None
    nelIndexedTraversal.find(_._1 >= 1)(nel) shouldEqual 1.some
    nelIndexedTraversal.find(greaterThan5 compose Tuple2._1)(nel) shouldEqual None
    nelIndexedTraversal.find(greaterThan10 compose Tuple2._1)(nel) shouldEqual None
  }

  test("first") {
    nelFromTraversalWithIndex.first(nel) shouldEqual (1, 0).some
    fromTraverse.first(nel) shouldEqual (1, 0).some
    nelIndexedTraversal.first(nel) shouldEqual (1, 0).some
  }

  test("last") {
    nelFromTraversalWithIndex.last(nel) shouldEqual (6, 5).some
    fromTraverse.last(nel) shouldEqual (6, 5).some
    nelIndexedTraversal.last(nel) shouldEqual (1, 0).some
  }

  test("minimum") {
    nelFromTraversalWithIndex.minimum(nel) shouldEqual indexedNel.head._1.some
    fromTraverse.minimum(nel) shouldEqual indexedNel.head._1.some
    nelIndexedTraversal.minimum(nel) shouldEqual nel.head.some
  }

  test("maximum") {
    nelFromTraversalWithIndex.maximum(nel) shouldEqual indexedNel.last._1.some
    fromTraverse.maximum(nel) shouldEqual indexedNel.last._1.some
    nelIndexedTraversal.maximum(nel) shouldEqual nel.head.some
  }

  test("toArray") {
    nelFromTraversalWithIndex.toArray(nel) shouldEqual nel.toList.toArray
    fromTraverse.toArray(nel) shouldEqual nel.toList.toArray
    nelIndexedTraversal.toArray(nel) shouldEqual Array(nel.head)
  }

  test("toList") {
    nelFromTraversalWithIndex.toList(nel) shouldEqual nel.toList
    fromTraverse.toList(nel) shouldEqual nel.toList
    nelIndexedTraversal.toList(nel) shouldEqual List(nel.head)
  }

  test("use") {
    nelFromTraversalWithIndex.use.runA(nel).value shouldEqual indexedNel.toList
    fromTraverse.use.runA(nel).value shouldEqual indexedNel.toList
    nelIndexedTraversal.use.runA(nel).value shouldEqual List((1, 0))
  }

  test("reindex") {
    nelFromTraversalWithIndex
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
    listFromTraversalWithIndex.filterByIndex(_ < 3).viewAll(list) shouldEqual indexedList.take(3)
  }

  test("element") {
    listFromTraversalWithIndex.element(1).preview(list) shouldEqual 2.some
  }
}
