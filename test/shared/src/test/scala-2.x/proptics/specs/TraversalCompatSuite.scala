package proptics.specs

import spire.std.boolean._
import spire.std.int._

import proptics.Traversal

trait TraversalCompatSuite extends PropticsSuite {
  val wholeTraversal: Traversal[Whole, Int]
  val fromTraverse: Traversal[List[Int], Int]
  val boolTraversal: Traversal[List[Boolean], Boolean]

  test("sum") {
    fromTraverse.sum(list) shouldEqual list.sum
    wholeTraversal.sum(whole9) shouldEqual 9
  }

  test("product") {
    fromTraverse.product(list) shouldEqual list.product
    fromTraverse.product(listEmpty) shouldEqual 1
    wholeTraversal.product(whole9) shouldEqual 9
  }

  test("forall") {
    fromTraverse.forall(_ < 10)(list) shouldEqual true
    fromTraverse.forall(_ < 10)(listEmpty) shouldEqual true
    fromTraverse.forall(_ > 10)(list) shouldEqual false
    fromTraverse.forall(_ > 10)(listEmpty) shouldEqual true
    wholeTraversal.forall(_ < 10)(whole9) shouldEqual true
    wholeTraversal.forall(_ > 10)(whole9) shouldEqual false
  }

  test("forall using heyting") {
    fromTraverse.forall(list)(_ < 10) shouldEqual true
    fromTraverse.forall(listEmpty)(_ < 10) shouldEqual true
    fromTraverse.forall(list)(_ > 10) shouldEqual false
    fromTraverse.forall(listEmpty)(_ > 10) shouldEqual true
    wholeTraversal.forall(whole9)(_ < 10) shouldEqual true
    wholeTraversal.forall(whole9)(_ > 10) shouldEqual false
  }

  test("and") {
    boolTraversal.and(boolList) shouldEqual false
    boolTraversal.and(boolTraversal.set(true)(boolList)) shouldEqual true
    boolTraversal.and(falseBoolList) shouldEqual false
  }

  test("or") {
    boolTraversal.or(boolList) shouldEqual true
    boolTraversal.or(falseBoolList) shouldEqual false
  }

  test("any") {
    fromTraverse.any(list)(greaterThan5) shouldEqual true
    fromTraverse.any(listEmpty)(greaterThan10) shouldEqual false
    wholeTraversal.any(whole9)(greaterThan5) shouldEqual true
  }

  test("exist") {
    fromTraverse.exists(greaterThan5)(list) shouldEqual true
    fromTraverse.exists(greaterThan10)(list) shouldEqual false
    wholeTraversal.exists(greaterThan5)(whole9) shouldEqual true
    wholeTraversal.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    fromTraverse.notExists(greaterThan5)(list) shouldEqual false
    fromTraverse.notExists(greaterThan10)(list) shouldEqual true
    fromTraverse.notExists(greaterThan10)(list) shouldEqual !fromTraverse.exists(greaterThan10)(list)
    wholeTraversal.notExists(greaterThan5)(whole9) shouldEqual false
    wholeTraversal.notExists(greaterThan10)(whole9) shouldEqual true
    wholeTraversal.notExists(greaterThan10)(whole9) shouldEqual !wholeTraversal.exists(greaterThan10)(whole9)
  }

  test("contains") {
    fromTraverse.contains(5)(list) shouldEqual true
    fromTraverse.contains(10)(list) shouldEqual false
    wholeTraversal.contains(9)(whole9) shouldEqual true
    wholeTraversal.contains(10)(whole9) shouldEqual false
  }

  test("notContains") {
    fromTraverse.notContains(5)(list) shouldEqual false
    fromTraverse.notContains(10)(list) shouldEqual true
    fromTraverse.notContains(10)(list) shouldEqual !fromTraverse.contains(10)(list)
    wholeTraversal.notContains(9)(whole9) shouldEqual false
    wholeTraversal.notContains(10)(whole9) shouldEqual true
    wholeTraversal.notContains(10)(whole9) shouldEqual !wholeTraversal.contains(10)(whole9)
  }
}
