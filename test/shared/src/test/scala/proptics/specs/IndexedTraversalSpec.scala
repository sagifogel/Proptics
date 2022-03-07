package proptics.specs

import scala.Function.const

import cats.data.NonEmptyList
import cats.syntax.foldable._
import cats.syntax.option._

import proptics._
import proptics.instances.traverseWithIndex._
import proptics.law.discipline._
import proptics.specs.compose._
import proptics.syntax.indexedTraversal._
import proptics.syntax.tuple._

class IndexedTraversalSpec extends IndexedTraversalCompatSuite {
  val indexedNel: NonEmptyList[(Int, Int)] = nel.zipWithIndex
  val nelTail: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(nel.tail)
  val singletonNel: NonEmptyList[Int] = NonEmptyList.one(nel.head)
  val idxNelTail: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(nel.tail)
  val idxSingletonNel: NonEmptyList[Int] = NonEmptyList.one(nel.head)
  val wholeTraversal: IndexedTraversal[Int, Whole, Int] =
    IndexedTraversal[Int, Whole, Int](whole => (whole.part, 0))(whole => part => whole.copy(part = part))
  val nelIndexedTraversal: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal[Int, NonEmptyList[Int], Int](nel => (nel.head, 0))(nel => i => nel.copy(head = i))
  val nelFromTraversalWithIndex: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal.fromTraverseWithIndex[NonEmptyList, Int, Int]
  val listFromTraversalWithIndex: IndexedTraversal[Int, List[Int], Int] =
    IndexedTraversal.fromTraverseWithIndex[List, Int, Int]
  val mapFromTraversalWithIndex: IndexedTraversal[Int, Map[Int, Int], Int] =
    IndexedTraversal.fromTraverseWithIndex[Map[Int, *], Int, Int]
  val boolIndexedTraversalWithIndex: IndexedTraversal[Int, NonEmptyList[Boolean], Boolean] =
    IndexedTraversal.fromTraverseWithIndex[NonEmptyList, Int, Boolean]
  val fromTraverse: IndexedTraversal[Int, NonEmptyList[Int], Int] =
    IndexedTraversal.fromTraverse[NonEmptyList, Int]

  checkAll("IndexedTraversal[Int, Whole, Int] asTraversal", TraversalTests(wholeTraversal.asTraversal).traversal)
  checkAll("IndexedTraversal[Int, NonEmptyList[Int], Int] apply", IndexedTraversalTests(nelIndexedTraversal).indexedTraversal)
  checkAll("IndexedTraversal[Int, NonEmptyList[Int], Int] fromTraverse", IndexedTraversalTests(fromTraverse).indexedTraversal)
  checkAll("IndexedTraversal[Int, List[Int], Int] fromTraverseWithIndex", IndexedTraversalTests(listFromTraversalWithIndex).indexedTraversal)
  checkAll("IndexedTraversal[Int, NonEmptyList[Int], Int] fromTraverseWithIndex", IndexedTraversalTests(nelFromTraversalWithIndex).indexedTraversal)
  checkAll("IndexedTraversal[Int, Map[Int, *], Int] fromTraverseWithIndex", IndexedTraversalTests(mapFromTraversalWithIndex).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] compose with Iso[Int, Int]", IndexedTraversalTests(indexedTraversal compose Iso.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with Iso[Int, Int]", IndexedTraversalTests(indexedTraversal andThen Iso.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] compose with AnIso[Int, Int]", IndexedTraversalTests(indexedTraversal compose AnIso.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with AnIso[Int, Int]", IndexedTraversalTests(indexedTraversal andThen AnIso.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] compose with Lens[Int, Int]", IndexedTraversalTests(indexedTraversal compose Lens.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with Lens[Int, Int]", IndexedTraversalTests(indexedTraversal andThen Lens.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] compose with ALens[Int, Int]", IndexedTraversalTests(indexedTraversal compose ALens.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with ALens[Int, Int]", IndexedTraversalTests(indexedTraversal andThen ALens.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] compose with Prism[Int, Int]", IndexedTraversalTests(indexedTraversal compose Prism.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with Prism[Int, Int]", IndexedTraversalTests(indexedTraversal andThen Prism.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] compose with APrism[Int, Int]", IndexedTraversalTests(indexedTraversal compose APrism.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with APrism[Int, Int]", IndexedTraversalTests(indexedTraversal andThen APrism.id[Int]).indexedTraversal)
  checkAll(
    "IndexedTraversal[Int, Int, Int] compose with AffineTraversal[Int, Int]",
    IndexedTraversalTests(indexedTraversal compose AffineTraversal.id[Int]).indexedTraversal
  )
  checkAll(
    "IndexedTraversal[Int, Int, Int] andThen with AffineTraversal[Int, Int]",
    IndexedTraversalTests(indexedTraversal andThen AffineTraversal.id[Int]).indexedTraversal
  )
  checkAll(
    "IndexedTraversal[Int, Int, Int] compose with AnAffineTraversal[Int, Int]",
    IndexedTraversalTests(indexedTraversal compose AnAffineTraversal.id[Int]).indexedTraversal
  )
  checkAll(
    "IndexedTraversal[Int, Int, Int] andThen with AnAffineTraversal[Int, Int]",
    IndexedTraversalTests(indexedTraversal andThen AnAffineTraversal.id[Int]).indexedTraversal
  )
  checkAll("IndexedTraversal[Int, Int, Int] compose with Traversal[Int, Int]", IndexedTraversalTests(indexedTraversal compose Traversal.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with Traversal[Int, Int]", IndexedTraversalTests(indexedTraversal andThen Traversal.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] compose with ATraversal[Int, Int]", IndexedTraversalTests(indexedTraversal compose ATraversal.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with ATraversal[Int, Int]", IndexedTraversalTests(indexedTraversal andThen ATraversal.id[Int]).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] compose with Setter[Int, Int]", IndexedSetterTests(indexedTraversal compose Setter.id[Int]).indexedSetter)
  checkAll("IndexedTraversal[Int, Int, Int] andThen with Setter[Int, Int]", IndexedSetterTests(indexedTraversal andThen Setter.id[Int]).indexedSetter)
  checkAll("IndexedTraversal[Int, Int, Int] <<* IndexedLens[Int, Int]", IndexedTraversalTests(indexedTraversal <<* indexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] *>> IndexedLens[Int, Int, Int]", IndexedTraversalTests(indexedTraversal *>> indexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] <<* AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(indexedTraversal <<* anIndexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] *>> AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(indexedTraversal *>> anIndexedLens).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] <<* IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(indexedTraversal <<* indexedTraversal).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] *>> IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(indexedTraversal *>> indexedTraversal).indexedTraversal)
  checkAll("IndexedTraversal[Int, Int, Int] <<* IndexedSetter[Int, Int, Int]", IndexedSetterTests(indexedTraversal <<* indexedSetter).indexedSetter)
  checkAll("IndexedTraversal[Int, Int, Int] *>> IndexedSetter[Int, Int, Int]", IndexedSetterTests(indexedTraversal *>> indexedSetter).indexedSetter)

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
    nelFromTraversalWithIndex.find(greaterThan5 compose Tuple2._1)(nel) shouldEqual list.find(greaterThan5).map((_, 5))
    nelFromTraversalWithIndex.find(greaterThan10 compose Tuple2._1)(nel) shouldEqual None
    fromTraverse.find(greaterThan5 compose Tuple2._1)(nel) shouldEqual list.find(greaterThan5).map((_, 5))
    fromTraverse.find(greaterThan10 compose Tuple2._1)(nel) shouldEqual None
    nelIndexedTraversal.find(_._1 >= 1)(nel) shouldEqual (1, 0).some
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
      .viewAll(nel) shouldEqual indexedList.map { case (a, i) => (a, i.toString) }
  }

  test("andThen with Fold") {
    val composed = listFromTraversalWithIndex andThen Fold.id[Int]
    composed.foldMap(List(0, 1, 2)) { case (_, i) => List(i) } shouldEqual List(0, 1, 2)
  }

  test("compose with Fold") {
    val composed = listFromTraversalWithIndex compose Fold.id[List[Int]]
    composed.foldMap(List(0, 1, 2)) { case (_, i) => List(i) } shouldEqual List(0, 1, 2)
  }

  test("compose with IndexedGetter with right index") {
    val composed = IndexedTraversal[Int, Int, Int]((_, 1))(const(identity)) *>> indexedGetter

    composed.viewAll(9) shouldEqual List((9, 0))
  }

  test("compose with IndexedGetter with left index") {
    val composed = IndexedTraversal[Int, Int, Int]((_, 1))(const(identity)) <<* indexedGetter

    composed.viewAll(9) shouldEqual List((9, 1))
  }

  test("compose with IndexedFold with right index") {
    val composed = IndexedTraversal[Int, Int, Int]((_, 1))(const(identity)) *>> indexedFold

    composed.viewAll(9) shouldEqual List((9, 0))
  }

  test("compose with IndexedFold with left index") {
    val composed = IndexedTraversal[Int, Int, Int]((_, 1))(const(identity)) <<* indexedFold

    composed.viewAll(9) shouldEqual List((9, 1))
  }

  test("filterByIndex") {
    listFromTraversalWithIndex.filterByIndex(_ < 3).viewAll(list) shouldEqual indexedList.take(3)
  }

  test("elementAt") {
    listFromTraversalWithIndex.elementAt(1).preview(list) shouldEqual (2, 1).some
  }
}
