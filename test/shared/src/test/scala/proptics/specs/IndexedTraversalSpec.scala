package proptics.specs

import cats.data.NonEmptyList
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.foldable._
import cats.syntax.option._
import proptics.law.{IndexedSetterTests, IndexedTraversalTests, TraversalTests}
import proptics.syntax.tuple._
import proptics.{IndexedTraversal, IndexedTraversal_}
import spire.std.boolean._
import proptics.specs.compose._

import scala.Function.const

class IndexedTraversalSpec extends PropticsSuite {
  val indexedNel: NonEmptyList[(Int, Int)] = nel.zipWithIndex.map(_.swap)
  val nelBool: NonEmptyList[(Int, Boolean)] = NonEmptyList.fromListUnsafe(boolList).zipWithIndex.map(_.swap)
  val nelFalseBool: NonEmptyList[(Int, Boolean)] = NonEmptyList.fromListUnsafe(falseBoolList).zipWithIndex.map(_.swap)

  val wholeTraversal: IndexedTraversal[Int, Whole, Int] =
    IndexedTraversal[Int, Whole, Int](whole => (0, whole.part))(whole => part => whole.copy(part = part))

  val nelIndexedTraversal: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal[Int, NonEmptyList[Int], Int](nel => (0, nel.head))(nel => i => nel.copy(head = i))

  val fromTraversal: IndexedTraversal_[Int, NonEmptyList[(Int, Int)], NonEmptyList[Int], Int, Int] =
    IndexedTraversal.fromTraverse[NonEmptyList, Int, Int]

  val boolIndexedTraversal: IndexedTraversal_[Int, NonEmptyList[(Int, Boolean)], NonEmptyList[Boolean], Boolean, Boolean] =
    IndexedTraversal.fromTraverse[NonEmptyList, Int, Boolean]

  checkAll("IndexedTraversal[Int, NonEmptyList[Int], Int] apply", IndexedTraversalTests(nelIndexedTraversal).indexedTraversal)
  checkAll("IndexedTraversal[Int, Whole, Int] asTraversal", TraversalTests(wholeTraversal.asTraversal).traversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedLens[Int, Int]", IndexedTraversalTests(indexedTraversal compose indexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with AnIndexedLens[Int, Int]", IndexedTraversalTests(indexedTraversal compose anIndexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedTraversal[Int, Int]", IndexedTraversalTests(indexedTraversal compose indexedTraversal).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int] compose with IndexedSetter[Int, Int]", IndexedSetterTests(indexedTraversal compose indexedSetter).indexedSetter)

  test("viewAll") {
    fromTraversal.viewAll(indexedNel) shouldEqual indexedNel.toList
    nelIndexedTraversal.viewAll(nel) shouldEqual List((0, 1))
  }

  test("preview") {
    fromTraversal.preview(indexedNel) shouldEqual Some((0, 1))
    nelIndexedTraversal.preview(nel) shouldEqual Some((0, 1))
  }

  test("set") {
    fromTraversal.set(0)(indexedNel) shouldEqual nel.map(const(0))
    nelIndexedTraversal.set(0)(nel) shouldEqual nel.copy(head = 0)
  }

  test("over") {
    fromTraversal.over(_._2 + 1)(indexedNel) shouldEqual nel.map(_ + 1)
    nelIndexedTraversal.over(oneToNine)(nel) shouldEqual nel.copy(head = 9)
  }

  test("traverse") {
    fromTraversal.traverse(indexedNel)(_._2.some) shouldEqual nel.some
    fromTraversal.traverse(indexedNel)(const(none[Int])) shouldEqual None
    fromTraversal.traverse(indexedNel)(_._2.some) shouldEqual fromTraversal.overF(_._2.some)(indexedNel)
    nelIndexedTraversal.traverse(nel)(_._2.some) shouldEqual nel.some
  }

  test("foldMap") {
    fromTraversal.foldMap(indexedNel)(_._2.toString) shouldEqual nel.map(_.toString).intercalate("")
    nelIndexedTraversal.foldMap(nel)(_._2.toString) shouldEqual nel.head.toString
  }

  test("foldr") {
    fromTraversal.foldr(indexedNel ++ List((indexedNel.length, 20)))(0)(_._2 - _) should be > 0
    nelIndexedTraversal.foldr(nel ++ List(20))(0)(_._2 - _) should be > 0
  }

  test("foldl") {
    fromTraversal.foldl(indexedNel ++ List((indexedNel.length, 20)))(0)(_ - _._2) should be < 0
    nelIndexedTraversal.foldl(nel ++ List(20))(0)(_ - _._2) should be < 0
  }

  test("sequence_") {
    fromTraversal.sequence_[Option](indexedNel) shouldEqual ().some
    nelIndexedTraversal.sequence_[Option](nel) shouldEqual ().some
  }

  test("traverse_") {
    fromTraversal.traverse_(indexedNel)(_._2.some) shouldEqual Some(())
    fromTraversal.traverse_(indexedNel)(const(None)) shouldEqual None
    nelIndexedTraversal.traverse_(nel)(_._2.some) shouldEqual Some(())
    nelIndexedTraversal.traverse_(nel)(const(None)) shouldEqual None
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromTraversal.sum(indexedNel) shouldEqual list.sum
      nelIndexedTraversal.sum(nel) shouldEqual list.head
    }

    test("product") {
      fromTraversal.product(indexedNel) shouldEqual list.product
      nelIndexedTraversal.product(nel) shouldEqual list.head
    }
  }

  test("forall") {
    fromTraversal.forall(_._2 < 10)(indexedNel) shouldEqual true
    fromTraversal.forall(_._2 > 10)(indexedNel) shouldEqual false
    nelIndexedTraversal.forall(_._2 < 10)(nel) shouldEqual true
    nelIndexedTraversal.forall(_._2 > 10)(nel) shouldEqual false
  }

  test("forall using heyting") {
    fromTraversal.forall(indexedNel)(_._2 < 10) shouldEqual true
    fromTraversal.forall(indexedNel)(_._2 > 10) shouldEqual false
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
    nelIndexedTraversal.any(nel)(greaterThan5 compose Tuple2._2) shouldEqual false
  }

  test("exist") {
    fromTraversal.exists(greaterThan5 compose Tuple2._2)(indexedNel) shouldEqual true
    fromTraversal.exists(greaterThan10 compose Tuple2._2)(indexedNel) shouldEqual false
    nelIndexedTraversal.exists(greaterThan5 compose Tuple2._2)(nel) shouldEqual false
    nelIndexedTraversal.exists(greaterThan5 compose Tuple2._2)(nel.copy(head = 10)) shouldEqual true
    nelIndexedTraversal.exists(greaterThan10 compose Tuple2._2)(nel.copy(head = 20)) shouldEqual true
  }

  test("notExists") {
    fromTraversal.notExists(greaterThan5 compose Tuple2._2)(indexedNel) shouldEqual false
    fromTraversal.notExists(greaterThan10 compose Tuple2._2)(indexedNel) shouldEqual true
    fromTraversal.notExists(greaterThan10 compose Tuple2._2)(indexedNel) shouldEqual
      !fromTraversal.exists(greaterThan10 compose Tuple2._2)(indexedNel)
    nelIndexedTraversal.notExists(greaterThan5 compose Tuple2._2)(nel) shouldEqual true
    nelIndexedTraversal.notExists(greaterThan10 compose Tuple2._2)(nel) shouldEqual true
    nelIndexedTraversal.notExists(greaterThan10 compose Tuple2._2)(nel) shouldEqual
      !nelIndexedTraversal.exists(greaterThan10 compose Tuple2._2)(nel)
  }

  test("contains") {
    fromTraversal.contains(indexedNel)((0, 1)) shouldEqual true
    fromTraversal.contains(indexedNel)((4, 5)) shouldEqual true
    fromTraversal.contains(indexedNel)((0, 10)) shouldEqual false
    fromTraversal.contains(indexedNel)((1, 5)) shouldEqual false
    nelIndexedTraversal.contains(nel)((0, 1)) shouldEqual true
    nelIndexedTraversal.contains(nel)((0, 10)) shouldEqual false
    nelIndexedTraversal.contains(nel)((1, 9)) shouldEqual false
  }

  test("notContains") {
    fromTraversal.notContains(indexedNel)((0, 1)) shouldEqual false
    fromTraversal.notContains(indexedNel)((4, 5)) shouldEqual false
    fromTraversal.notContains(indexedNel)((0, 10)) shouldEqual true
    fromTraversal.notContains(indexedNel)((1, 5)) shouldEqual true
    fromTraversal.notContains(indexedNel)((0, 10)) shouldEqual !fromTraversal.contains(indexedNel)((0, 10))
    nelIndexedTraversal.notContains(nel)((0, 1)) shouldEqual false
    nelIndexedTraversal.notContains(nel)((0, 10)) shouldEqual true
    nelIndexedTraversal.notContains(nel)((1, 5)) shouldEqual true
    nelIndexedTraversal.notContains(nel)((0, 10)) shouldEqual !nelIndexedTraversal.contains(nel)((0, 10))
  }

  test("isEmpty") {
    fromTraversal.isEmpty(indexedNel) shouldEqual false
    nelIndexedTraversal.isEmpty(nel) shouldEqual false
  }

  test("nonEmpty") {
    fromTraversal.nonEmpty(indexedNel) shouldEqual true
    fromTraversal.nonEmpty(indexedNel) shouldEqual !fromTraversal.isEmpty(indexedNel)
    nelIndexedTraversal.nonEmpty(nel) shouldEqual true
    nelIndexedTraversal.nonEmpty(nel) shouldEqual !nelIndexedTraversal.isEmpty(nel)
  }

  test("length") {
    fromTraversal.length(indexedNel) shouldEqual indexedNel.length
    nelIndexedTraversal.length(nel) shouldEqual 1
  }

  test("find") {
    fromTraversal.find(greaterThan5 compose Tuple2._2)(indexedNel) shouldEqual list.find(greaterThan5)
    fromTraversal.find(greaterThan10 compose Tuple2._2)(indexedNel) shouldEqual None
    nelIndexedTraversal.find(_._2 >= 1)(nel) shouldEqual 1.some
    nelIndexedTraversal.find(greaterThan5 compose Tuple2._2)(nel) shouldEqual None
    nelIndexedTraversal.find(greaterThan10 compose Tuple2._2)(nel) shouldEqual None
  }

  test("first") {
    fromTraversal.first(indexedNel) shouldEqual (0, 1).some
    nelIndexedTraversal.first(nel) shouldEqual (0, 1).some
  }

  test("last") {
    fromTraversal.last(indexedNel) shouldEqual (5, 6).some
    nelIndexedTraversal.last(nel) shouldEqual (0, 1).some
  }

  test("minimum") {
    fromTraversal.minimum(indexedNel) shouldEqual indexedNel.head._2.some
    nelIndexedTraversal.minimum(nel) shouldEqual nel.head.some
  }

  test("maximum") {
    fromTraversal.maximum(indexedNel) shouldEqual indexedNel.last._2.some
    nelIndexedTraversal.maximum(nel) shouldEqual nel.head.some
  }

  test("toArray") {
    fromTraversal.toArray(indexedNel) shouldEqual indexedNel.toList.toArray
    nelIndexedTraversal.toArray(nel) shouldEqual Array((0, nel.head))
  }

  test("toList") {
    fromTraversal.toList(indexedNel) shouldEqual indexedNel.toList
    nelIndexedTraversal.toList(nel) shouldEqual List((0, nel.head))
  }

  test("use") {
    fromTraversal.use.runA(indexedNel).value shouldEqual indexedNel.toList
    nelIndexedTraversal.use.runA(nel).value shouldEqual List((0, 1))
  }

  test("compose with IndexedGetter") {
    (indexedTraversal compose indexedGetter).foldMap(9)(_._2) shouldEqual 9
  }

  test("compose with IndexedFold") {
    (indexedTraversal compose indexedFold).foldMap(9)(_._2) shouldEqual 9
  }
}
