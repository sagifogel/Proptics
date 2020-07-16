package proptics.specs

import cats.data.State
import spire.std.boolean._
import cats.instances.int._
import cats.syntax.option._
import cats.instances.list._
import proptics.{Fold, Fold_}

import scala.Function.const
import scala.util.Random

class FoldSpec extends PropticsSuite {
  val fromFoldable: Fold_[List[Int], Int, Int, Int] = Fold_.fromFoldable
  val boolFoldable: Fold[List[Boolean], Boolean] = Fold.fromFoldable
  val foldable: Fold[Whole, Int] = Fold[Whole, Int](_.focus)

  test("viewAll") {
    fromFoldable.viewAll(list) shouldEqual list
    fromFoldable.viewAll(emptyList) shouldEqual emptyList
    foldable.viewAll(whole9) shouldEqual List(whole9.focus)
  }

  test("preview") {
    fromFoldable.preview(list) shouldEqual 1.some
    fromFoldable.preview(emptyList) shouldEqual None
    foldable.preview(whole9) shouldEqual 9.some
  }

  test("foldMap") {
    fromFoldable.foldMap(list)(identity) shouldEqual list.sum
    fromFoldable.foldMap(list)(List(_)) shouldEqual list
    fromFoldable.foldMap(emptyList)(identity) shouldEqual 0
    fromFoldable.foldMap(emptyList)(List(_)) shouldEqual emptyList
    foldable.foldMap(whole9)(identity) shouldEqual 9
  }

  test("fold") {
    fromFoldable.fold(list) shouldEqual list.sum
    fromFoldable.fold(list) shouldEqual fromFoldable.view(list)
    fromFoldable.fold(emptyList) shouldEqual 0
    fromFoldable.fold(emptyList) shouldEqual fromFoldable.view(emptyList)
    foldable.fold(whole9) shouldEqual 9
    foldable.fold(whole9) shouldEqual foldable.view(whole9)
  }

  test("foldr") {
    fromFoldable.foldr(list)(0)(_ + _) shouldEqual list.sum
    fromFoldable.foldr(list)(0)(_ + _) should be > 0
    fromFoldable.foldr(list ++ List(20))(0)(_ - _) should be > 0
    fromFoldable.foldr(emptyList)(0)(_ + _) shouldEqual 0
    fromFoldable.foldr(emptyList)(0)(_ - _) shouldEqual 0
    foldable.foldr(whole9)(1)(_ + _) shouldEqual 10
    foldable.foldr(whole9)(1)(_ - _) shouldEqual 8
  }

  test("foldl") {
    fromFoldable.foldl(list)(0)(_ + _) shouldEqual list.sum
    fromFoldable.foldl(list)(0)(_ + _) should be > 0
    fromFoldable.foldl(list ++ List(20))(0)(_ - _) should be < 0
    fromFoldable.foldl(emptyList)(0)(_ + _) shouldEqual 0
    fromFoldable.foldl(emptyList)(0)(_ - _) shouldEqual 0
    foldable.foldl(whole9)(1)(_ + _) shouldEqual 10
    foldable.foldl(whole9)(1)(_ - _) shouldEqual -8
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromFoldable.sum(list) shouldEqual list.sum
      foldable.sum(whole9) shouldEqual 9
    }

    test("product") {
      fromFoldable.product(list) shouldEqual list.product
      fromFoldable.product(emptyList) shouldEqual 1
      foldable.product(whole9) shouldEqual 9
    }
  }

  test("forall") {
    fromFoldable.forall(_ < 10)(list) shouldEqual true
    fromFoldable.forall(_ < 10)(emptyList) shouldEqual true
    fromFoldable.forall(_ > 10)(list) shouldEqual false
    fromFoldable.forall(_ > 10)(emptyList) shouldEqual true
    foldable.forall(_ < 10)(whole9) shouldEqual true
    foldable.forall(_ > 10)(whole9) shouldEqual false
  }

  test("forall using heyting") {
    fromFoldable.forall(list)(_ < 10) shouldEqual true
    fromFoldable.forall(emptyList)(_ < 10) shouldEqual true
    fromFoldable.forall(list)(_ > 10) shouldEqual false
    fromFoldable.forall(emptyList)(_ > 10) shouldEqual true
    foldable.forall(whole9)(_ < 10) shouldEqual true
    foldable.forall(whole9)(_ > 10) shouldEqual false
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
  }

  test("exists") {
    fromFoldable.exists(greaterThan5)(list) shouldEqual true
    fromFoldable.exists(greaterThan10)(list) shouldEqual false
    foldable.exists(greaterThan5)(whole9) shouldEqual true
    foldable.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    fromFoldable.notExists(greaterThan5)(list) shouldEqual false
    fromFoldable.notExists(greaterThan10)(list) shouldEqual true
    fromFoldable.notExists(greaterThan10)(list) shouldEqual !fromFoldable.exists(greaterThan10)(list)
    foldable.notExists(greaterThan5)(whole9) shouldEqual false
    foldable.notExists(greaterThan10)(whole9) shouldEqual true
    foldable.notExists(greaterThan10)(whole9) shouldEqual !foldable.exists(greaterThan10)(whole9)
  }

  test("contains") {
    fromFoldable.contains(list)(5) shouldEqual true
    fromFoldable.contains(list)(10) shouldEqual false
    foldable.contains(whole9)(9) shouldEqual true
    foldable.contains(whole9)(10) shouldEqual false
  }

  test("notContains") {
    fromFoldable.notContains(list)(5) shouldEqual false
    fromFoldable.notContains(list)(10) shouldEqual true
    fromFoldable.notContains(list)(10) shouldEqual !fromFoldable.contains(list)(10)
    foldable.notContains(whole9)(9) shouldEqual false
    foldable.notContains(whole9)(10) shouldEqual true
    foldable.notContains(whole9)(10) shouldEqual !foldable.contains(whole9)(10)
  }

  test("isEmpty") {
    fromFoldable.isEmpty(list) shouldEqual false
    fromFoldable.isEmpty(emptyList) shouldEqual true
    foldable.isEmpty(whole9) shouldEqual false
  }

  test("nonEmpty") {
    fromFoldable.nonEmpty(list) shouldEqual true
    fromFoldable.nonEmpty(emptyList) shouldEqual false
    fromFoldable.nonEmpty(list) shouldEqual !fromFoldable.isEmpty(list)
    foldable.nonEmpty(whole9) shouldEqual true
    foldable.nonEmpty(whole9) shouldEqual !foldable.isEmpty(whole9)
  }

  test("length") {
    fromFoldable.length(list) shouldEqual list.length
    fromFoldable.length(emptyList) shouldEqual 0
    foldable.length(whole9) shouldEqual 1
  }

  test("find") {
    fromFoldable.find(greaterThan5)(list) shouldEqual list.find(greaterThan5)
    fromFoldable.find(greaterThan10)(list) shouldEqual None
    foldable.find(greaterThan5)(whole9) shouldEqual 9.some
    foldable.find(greaterThan10)(whole9) shouldEqual None
  }

  test("first") {
    fromFoldable.first(list) shouldEqual list.head.some
    fromFoldable.first(emptyList) shouldEqual None
    foldable.first(whole9) shouldEqual 9.some
  }

  test("last") {
    fromFoldable.last(list) shouldEqual list.last.some
    fromFoldable.last(emptyList) shouldEqual None
    foldable.last(whole9) shouldEqual 9.some
  }

  test("minimum") {
    fromFoldable.minimum(Random.shuffle(list)) shouldEqual list.head.some
    fromFoldable.minimum(emptyList) shouldEqual None
    foldable.minimum(whole9) shouldEqual 9.some
  }

  test("maximum") {
    fromFoldable.maximum(Random.shuffle(list)) shouldEqual list.last.some
    fromFoldable.maximum(emptyList) shouldEqual None
    foldable.maximum(whole9) shouldEqual 9.some
  }

  test("toArray") {
    fromFoldable.toArray(list) shouldEqual list.toArray
    foldable.toArray(whole9) shouldEqual Array(9)
  }

  test("toList") {
    fromFoldable.toList(list) shouldEqual list
    foldable.toList(whole9) shouldEqual List(9)
  }

  test("use") {
    implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](1)

    fromFoldable.use.runA(list).value shouldEqual list
    foldable.use.runA(whole9).value shouldEqual List(9)
  }
}
