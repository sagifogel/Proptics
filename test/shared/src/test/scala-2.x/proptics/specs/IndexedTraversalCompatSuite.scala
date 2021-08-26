package proptics.specs

import cats.data.NonEmptyList
import proptics.IndexedTraversal

trait IndexedTraversalCompatSuite extends PropticsSuite {
  val wholeTraversal: IndexedTraversal[Int, Whole, Int]
  val fromTraverse: IndexedTraversal[Int, NonEmptyList[Int], Int]
  val listFromTraversalWithIndex: IndexedTraversal[Int, List[Int], Int]
  val nelIndexedTraversal: IndexedTraversal[Int, NonEmptyList[Int], Int]
  val mapFromTraversalWithIndex: IndexedTraversal[Int, Map[Int, Int], Int]
  val nelFromTraversalWithIndex: IndexedTraversal[Int, NonEmptyList[Int], Int]
  val boolIndexedTraversalWithIndex: IndexedTraversal[Int, NonEmptyList[Boolean], Boolean]

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
}
