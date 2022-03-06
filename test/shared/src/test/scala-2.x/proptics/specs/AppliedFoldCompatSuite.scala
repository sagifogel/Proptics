package proptics.specs

import spire.std.boolean._
import spire.std.int._

import proptics.AppliedFold
import proptics.syntax.applied.fold._

trait AppliedFoldCompatSuite extends PropticsSuite {
  val listFoldable: AppliedFold[List[Int], Int]
  val emptyFoldable: AppliedFold[List[Int], Int]
  val negativeBoolFoldable: AppliedFold[List[Boolean], Boolean]
  val positiveBoolFoldable: AppliedFold[List[Boolean], Boolean]

  test("sum") {
    listFoldable.sum shouldEqual list.sum
  }

  test("product") {
    listFoldable.product shouldEqual list.product
    emptyFoldable.product shouldEqual 1
  }

  test("forall") {
    listFoldable.forall(_ < 10) shouldEqual true
    emptyFoldable.forall(_ < 10) shouldEqual true
    listFoldable.forall(_ > 10) shouldEqual false
    emptyFoldable.forall(_ > 10) shouldEqual true
  }

  test("forall using heyting") {
    listFoldable.forallH(_ < 10) shouldEqual true
    emptyFoldable.forallH(_ < 10) shouldEqual true
    listFoldable.forallH(_ > 10) shouldEqual false
    emptyFoldable.forallH(_ > 10) shouldEqual true
  }

  test("and") {
    boolList.foldable_[Boolean].and shouldEqual false
    positiveBoolFoldable.and shouldEqual true
    negativeBoolFoldable.and shouldEqual false
  }

  test("or") {
    positiveBoolFoldable.or shouldEqual true
    negativeBoolFoldable.or shouldEqual false
  }

  test("any") {
    listFoldable.any(greaterThan5) shouldEqual true
    listFoldable.any(greaterThan10) shouldEqual false
  }

  test("exist") {
    listFoldable.exists(greaterThan5) shouldEqual true
    listFoldable.exists(greaterThan10) shouldEqual false
  }

  test("notExists") {
    listFoldable.notExists(greaterThan5) shouldEqual false
    listFoldable.notExists(greaterThan10) shouldEqual true
    listFoldable.notExists(greaterThan10) shouldEqual !listFoldable.exists(greaterThan10)
  }

  test("contains") {
    listFoldable.contains(5) shouldEqual true
    listFoldable.contains(10) shouldEqual false
  }

  test("notContains") {
    listFoldable.notContains(5) shouldEqual false
    listFoldable.notContains(10) shouldEqual true
    listFoldable.notContains(10) shouldEqual !listFoldable.contains(10)
  }
}
