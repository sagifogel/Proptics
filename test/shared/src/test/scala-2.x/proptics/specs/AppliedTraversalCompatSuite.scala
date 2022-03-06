package proptics.specs

import spire.std.boolean._
import spire.std.int._

import proptics.AppliedTraversal
import proptics.syntax.applied.traversal._

trait AppliedTraversalCompatSuite extends PropticsSuite {
  val listTraversal: AppliedTraversal[List[Int], Int]
  val emptyTraversal: AppliedTraversal[List[Int], Int]
  val positiveBoolTraversal: AppliedTraversal[List[Boolean], Boolean]
  val negativeBoolTraversal: AppliedTraversal[List[Boolean], Boolean]

  test("sum") {
    listTraversal.sum shouldEqual list.sum
  }

  test("product") {
    listTraversal.product shouldEqual list.product
    emptyTraversal.product shouldEqual 1
  }

  test("forall") {
    listTraversal.forall(_ < 10) shouldEqual true
    emptyTraversal.forall(_ < 10) shouldEqual true
    listTraversal.forall(_ > 10) shouldEqual false
    emptyTraversal.forall(_ > 10) shouldEqual true
  }

  test("forall using heyting") {
    listTraversal.forallH(_ < 10) shouldEqual true
    emptyTraversal.forallH(_ < 10) shouldEqual true
    listTraversal.forallH(_ > 10) shouldEqual false
    emptyTraversal.forallH(_ > 10) shouldEqual true
  }

  test("and") {
    boolList.traversal_[Boolean].and shouldEqual false
    positiveBoolTraversal.and shouldEqual true
    negativeBoolTraversal.and shouldEqual false
  }

  test("or") {
    positiveBoolTraversal.or shouldEqual true
    negativeBoolTraversal.or shouldEqual false
  }

  test("any") {
    listTraversal.any(greaterThan5) shouldEqual true
    listTraversal.any(greaterThan10) shouldEqual false
  }

  test("exist") {
    listTraversal.exists(greaterThan5) shouldEqual true
    listTraversal.exists(greaterThan10) shouldEqual false
  }

  test("notExists") {
    listTraversal.notExists(greaterThan5) shouldEqual false
    listTraversal.notExists(greaterThan10) shouldEqual true
    listTraversal.notExists(greaterThan10) shouldEqual !listTraversal.exists(greaterThan10)
  }

  test("contains") {
    listTraversal.contains(5) shouldEqual true
    listTraversal.contains(10) shouldEqual false
  }

  test("notContains") {
    listTraversal.notContains(5) shouldEqual false
    listTraversal.notContains(10) shouldEqual true
    listTraversal.notContains(10) shouldEqual !listTraversal.contains(10)
  }
}
