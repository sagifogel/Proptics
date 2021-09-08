package proptics.specs

import scala.Function.const

import algebra.instances.int._
import spire.std.boolean._

import proptics.IndexedFold
import proptics.syntax.tuple._

trait IndexedFoldCompatSuite extends PropticsSuite {
  val foldable: IndexedFold[Int, Whole, Int]
  val fromFoldable: IndexedFold[Int, List[Int], Int]
  val fromGetter: IndexedFold[Int, List[Int], List[Int]]
  val boolFoldable: IndexedFold[Int, List[Boolean], Boolean]
  val fromFoldableWithIndex: IndexedFold[Int, List[Int], Int]

  test("sum") {
    fromFoldableWithIndex.sum(list) shouldEqual list.sum
    fromFoldable.sum(list) shouldEqual list.sum
    foldable.sum(whole9) shouldEqual 9
  }

  test("product") {
    fromFoldableWithIndex.product(list) shouldEqual list.product
    fromFoldableWithIndex.product(emptyList) shouldEqual 1
    fromFoldable.product(list) shouldEqual list.product
    fromFoldable.product(emptyList) shouldEqual 1
    foldable.product(whole9) shouldEqual 9
  }

  test("forall") {
    fromFoldableWithIndex.forall(_._1 < 10)(list) shouldEqual true
    fromFoldableWithIndex.forall(_._1 < 10)(emptyList) shouldEqual true
    fromFoldableWithIndex.forall(_._1 > 10)(list) shouldEqual false
    fromFoldableWithIndex.forall(_._1 > 10)(emptyList) shouldEqual true
    fromFoldable.forall(_._1 < 10)(list) shouldEqual true
    fromFoldable.forall(_._1 < 10)(emptyList) shouldEqual true
    fromFoldable.forall(_._1 > 10)(list) shouldEqual false
    fromFoldable.forall(_._1 > 10)(emptyList) shouldEqual true
    foldable.forall(_._1 < 10)(whole9) shouldEqual true
    foldable.forall(_._1 > 10)(whole9) shouldEqual false
  }

  test("forall using heyting") {
    fromFoldableWithIndex.forall(list)(_._1 < 10) shouldEqual true
    fromFoldableWithIndex.forall(emptyList)(_._1 < 10) shouldEqual true
    fromFoldableWithIndex.forall(list)(_._1 > 10) shouldEqual false
    fromFoldableWithIndex.forall(emptyList)(_._1 > 10) shouldEqual true
    fromFoldable.forall(list)(_._1 < 10) shouldEqual true
    fromFoldable.forall(emptyList)(_._1 < 10) shouldEqual true
    fromFoldable.forall(list)(_._1 > 10) shouldEqual false
    fromFoldable.forall(emptyList)(_._1 > 10) shouldEqual true
    foldable.forall(whole9)(_._1 < 10) shouldEqual true
    foldable.forall(whole9)(_._1 > 10) shouldEqual false
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
    fromFoldableWithIndex.any(list)(greaterThan5 compose Tuple2._1) shouldEqual true
    fromFoldableWithIndex.any(emptyList)(greaterThan10 compose Tuple2._1) shouldEqual false
    fromFoldable.any(list)(greaterThan5 compose Tuple2._1) shouldEqual true
    fromFoldable.any(emptyList)(greaterThan10 compose Tuple2._1) shouldEqual false
    foldable.any(whole9)(greaterThan5 compose Tuple2._1) shouldEqual true
  }

  test("exists") {
    fromFoldableWithIndex.exists(greaterThan5 compose Tuple2._1)(list) shouldEqual true
    fromFoldableWithIndex.exists(greaterThan10 compose Tuple2._1)(list) shouldEqual false
    fromFoldable.exists(greaterThan5 compose Tuple2._1)(list) shouldEqual true
    fromFoldable.exists(greaterThan10 compose Tuple2._1)(list) shouldEqual false
    foldable.exists(greaterThan5 compose Tuple2._1)(whole9) shouldEqual true
    foldable.exists(greaterThan10 compose Tuple2._1)(whole9) shouldEqual false
  }

  test("notExists") {
    fromFoldableWithIndex.notExists(greaterThan5 compose Tuple2._1)(list) shouldEqual false
    fromFoldableWithIndex.notExists(greaterThan10 compose Tuple2._1)(list) shouldEqual true
    fromFoldableWithIndex.notExists(greaterThan10 compose Tuple2._1)(list) shouldEqual
      !fromFoldableWithIndex.exists(greaterThan10 compose Tuple2._1)(list)
    fromFoldable.notExists(greaterThan5 compose Tuple2._1)(list) shouldEqual false
    fromFoldable.notExists(greaterThan10 compose Tuple2._1)(list) shouldEqual true
    fromFoldable.notExists(greaterThan10 compose Tuple2._1)(list) shouldEqual
      !fromFoldable.exists(greaterThan10 compose Tuple2._1)(list)
    foldable.notExists(greaterThan5 compose Tuple2._1)(whole9) shouldEqual false
    foldable.notExists(greaterThan10 compose Tuple2._1)(whole9) shouldEqual true
    foldable.notExists(greaterThan10 compose Tuple2._1)(whole9) shouldEqual !foldable.exists(greaterThan10 compose Tuple2._1)(whole9)
  }

  test("contains") {
    fromFoldableWithIndex.contains((1, 0))(list) shouldEqual true
    fromFoldableWithIndex.contains((5, 4))(list) shouldEqual true
    fromFoldableWithIndex.contains((10, 0))(list) shouldEqual false
    fromFoldable.contains((1, 1))(list) shouldEqual false
    fromFoldable.contains((1, 0))(list) shouldEqual true
    fromFoldable.contains((5, 4))(list) shouldEqual true
    fromFoldable.contains((10, 0))(list) shouldEqual false
    fromFoldable.contains((1, 1))(list) shouldEqual false
    foldable.contains((9, 0))(whole9) shouldEqual true
    foldable.contains((10, 0))(whole9) shouldEqual false
    foldable.contains((9, 1))(whole9) shouldEqual false
  }

  test("notContains") {
    fromFoldableWithIndex.notContains((1, 0))(list) shouldEqual false
    fromFoldableWithIndex.notContains((5, 4))(list) shouldEqual false
    fromFoldableWithIndex.notContains((10, 1))(list) shouldEqual true
    fromFoldableWithIndex.notContains((1, 0))(list) shouldEqual
      !fromFoldableWithIndex.contains((1, 0))(list)
    fromFoldable.notContains((1, 0))(list) shouldEqual false
    fromFoldable.notContains((5, 4))(list) shouldEqual false
    fromFoldable.notContains((10, 1))(list) shouldEqual true
    fromFoldable.notContains((1, 0))(list) shouldEqual
      !fromFoldable.contains((1, 0))(list)
    foldable.notContains((9, 0))(whole9) shouldEqual false
    foldable.notContains((10, 0))(whole9) shouldEqual true
    foldable.notContains((9, 1))(whole9) shouldEqual true
    foldable.notContains((9, 0))(whole9) shouldEqual !foldable.contains((9, 0))(whole9)
  }
}
