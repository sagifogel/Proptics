package proptics.specs

import scala.Function.const

import spire.std.boolean._
import spire.std.int._

import proptics.Fold

trait FoldCompatSuite extends PropticsSuite {
  val foldable: Fold[Whole, Int]
  val fromFoldable: Fold[List[Int], Int]
  val fromGetter: Fold[List[Int], List[Int]]
  val boolFoldable: Fold[List[Boolean], Boolean]

  test("sum") {
    fromFoldable.sum(list) shouldEqual list.sum
    foldable.sum(whole9) shouldEqual 9
  }

  test("product") {
    fromFoldable.product(list) shouldEqual list.product
    fromFoldable.product(emptyList) shouldEqual 1
    foldable.product(whole9) shouldEqual 9
  }

  test("forall") {
    fromFoldable.forall(_ > 1)(List(2, 3, 4, 1)) shouldEqual false
    fromFoldable.forall(_ < 10)(list) shouldEqual true
    fromFoldable.forall(_ < 10)(emptyList) shouldEqual true
    fromFoldable.forall(_ > 10)(list) shouldEqual false
    fromFoldable.forall(_ > 10)(emptyList) shouldEqual true
    foldable.forall(_ < 10)(whole9) shouldEqual true
    foldable.forall(_ > 10)(whole9) shouldEqual false
    fromGetter.forall(_.forall(_ < 10))(list) shouldEqual true
    fromGetter.forall(_.forall(_ < 10))(emptyList) shouldEqual true
    fromGetter.forall(_.forall(_ > 10))(list) shouldEqual false
  }

  test("forall using heyting") {
    fromFoldable.forall(List(2, 3, 4, 1))(_ > 1) shouldEqual false
    fromFoldable.forall(list)(_ < 10) shouldEqual true
    fromFoldable.forall(emptyList)(_ < 10) shouldEqual true
    fromFoldable.forall(list)(_ > 10) shouldEqual false
    fromFoldable.forall(emptyList)(_ > 10) shouldEqual true
    foldable.forall(whole9)(_ < 10) shouldEqual true
    foldable.forall(whole9)(_ > 10) shouldEqual false
    fromGetter.forall(list)(_.forall(_ < 10)) shouldEqual true
    fromGetter.forall(emptyList)(_.forall(_ < 10)) shouldEqual true
    fromGetter.forall(list)(_.forall(_ > 10)) shouldEqual false
  }

  test("and") {
    boolFoldable.and(boolList) shouldEqual false
    boolFoldable.and(boolList.map(const(true))) shouldEqual true
    boolFoldable.and(falseBoolList) shouldEqual false
  }

  test("or") {
    boolFoldable.or(boolList) shouldEqual true
    boolFoldable.or(falseBoolList) shouldEqual false
  }

  test("any") {
    fromFoldable.any(list)(greaterThan5) shouldEqual true
    fromFoldable.any(emptyList)(greaterThan10) shouldEqual false
    foldable.any(whole9)(greaterThan5) shouldEqual true
    fromGetter.any(list)(_.exists(greaterThan5)) shouldEqual true
    fromGetter.any(list)(_.exists(greaterThan10)) shouldEqual false
  }

  test("exists") {
    fromFoldable.exists(greaterThan5)(list) shouldEqual true
    fromFoldable.exists(greaterThan10)(list) shouldEqual false
    foldable.exists(greaterThan5)(whole9) shouldEqual true
    foldable.exists(greaterThan10)(whole9) shouldEqual false
    fromGetter.exists(_.exists(greaterThan5))(list) shouldEqual true
    fromGetter.exists(_.exists(greaterThan10))(list) shouldEqual false
  }

  test("notExists") {
    fromFoldable.notExists(greaterThan5)(list) shouldEqual false
    fromFoldable.notExists(greaterThan10)(list) shouldEqual true
    fromFoldable.notExists(greaterThan10)(list) shouldEqual !fromFoldable.exists(greaterThan10)(list)
    foldable.notExists(greaterThan5)(whole9) shouldEqual false
    foldable.notExists(greaterThan10)(whole9) shouldEqual true
    foldable.notExists(greaterThan10)(whole9) shouldEqual !foldable.exists(greaterThan10)(whole9)
    fromGetter.notExists(_.exists(greaterThan5))(list) shouldEqual false
    fromGetter.notExists(_.exists(greaterThan10))(list) shouldEqual true
    fromGetter.notExists(_.exists(greaterThan10))(list) shouldEqual !fromGetter.exists(_.exists(greaterThan10))(list)
  }

  test("contains") {
    fromFoldable.contains(5)(list) shouldEqual true
    fromFoldable.contains(10)(list) shouldEqual false
    foldable.contains(9)(whole9) shouldEqual true
    foldable.contains(10)(whole9) shouldEqual false
    fromGetter.contains(list)(list) shouldEqual true
  }

  test("notContains") {
    fromFoldable.notContains(5)(list) shouldEqual false
    fromFoldable.notContains(10)(list) shouldEqual true
    fromFoldable.notContains(10)(list) shouldEqual !fromFoldable.contains(10)(list)
    foldable.notContains(9)(whole9) shouldEqual false
    foldable.notContains(10)(whole9) shouldEqual true
    foldable.notContains(10)(whole9) shouldEqual !foldable.contains(10)(whole9)
    fromGetter.notContains(list)(emptyList) shouldEqual true
    fromGetter.notContains(list)(list) shouldEqual false
    fromGetter.notContains(list)(list) shouldEqual !fromGetter.contains(list)(list)
  }
}
