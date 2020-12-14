package proptics.specs

import scala.Function.const
import scala.util.Random

import cats.data.State
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.option._
import spire.std.boolean._

import proptics.law.discipline._
import proptics.specs.compose._
import proptics.syntax.traversal._
import proptics.{Fold_, Getter, Lens, Prism, Traversal, Traversal_}

class TraversalSpec extends PropticsSuite {
  val plusOne: Int => Int = _ + 1
  val someEven: Int => Option[Int] = i => if (i % 2 == 0) i.some else none[Int]
  val fromTraversal: Traversal[List[Int], Int] = Traversal.fromTraverse[List, Int]
  val boolTraversal: Traversal[List[Boolean], Boolean] = Traversal.fromTraverse[List, Boolean]
  val wholeTraversal: Traversal[Whole, Int] = Traversal[Whole, Int](_.part)(whole => focus => whole.copy(part = focus))

  checkAll("Traversal[List[Int], Int] fromTraverse", TraversalTests(fromTraversal).traversal)
  checkAll("Traversal[Whole, Int] apply", TraversalTests(wholeTraversal).traversal)
  checkAll("Traversal[Int, Int] id", TraversalTests(Traversal.id[Int]).traversal)
  checkAll("Traversal[(Int, Int), (Int, Int), Int, Int] both", TraversalTests(Traversal_.both[(*, *), Int, Int]).traversal)
  checkAll("Traversal[List[Int], Int] element", TraversalTests(Traversal.element[List, Int](1)).traversal)
  checkAll("Traversal[List[Int], Int] take", TraversalTests(Traversal.take[List, Int](1)).traversal)
  checkAll("Traversal[List[Int], Int] drop", TraversalTests(Traversal.drop[List, Int](1)).traversal)
  checkAll("Traversal[Int, Int] compose with Iso[Int, Int]", TraversalTests(traversal compose iso).traversal)
  checkAll("Traversal[Int, Int] compose with AnIso[Int, Int]", TraversalTests(traversal compose anIso).traversal)
  checkAll("Traversal[Int, Int] compose with Lens[Int, Int]", TraversalTests(traversal compose lens).traversal)
  checkAll("Traversal[Int, Int] compose with ALens[Int, Int]", TraversalTests(traversal compose aLens).traversal)
  checkAll("Traversal[Int, Int] compose with Prism[Int, Int]", TraversalTests(traversal compose prism).traversal)
  checkAll("Traversal[Int, Int] compose with APrism[Int, Int]", TraversalTests(traversal compose aPrism).traversal)
  checkAll("Traversal[Int, Int] compose with AffineTraversal[Int, Int]", TraversalTests(traversal compose affineTraversal).traversal)
  checkAll("Traversal[Int, Int] compose with AnAffineTraversal[Int, Int]", TraversalTests(traversal compose anAffineTraversal).traversal)
  checkAll("Traversal[Int, Int] compose with Traversal[Int, Int]", TraversalTests(traversal compose traversal).traversal)
  checkAll("Traversal[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(traversal compose aTraversal).aTraversal)
  checkAll("Traversal[Int, Int] compose with Setter[Int, Int]", SetterTests(traversal compose setter).setter)

  test("viewAll") {
    fromTraversal.viewAll(list) shouldEqual list
    fromTraversal.viewAll(emptyList) shouldEqual emptyList
    wholeTraversal.viewAll(whole9) shouldEqual List(9)
  }

  test("preview") {
    fromTraversal.preview(list) shouldEqual Some(1)
    fromTraversal.preview(emptyList) shouldEqual None
    wholeTraversal.preview(whole9) shouldEqual Some(9)
  }

  test("set") {
    fromTraversal.set(0)(list) shouldEqual list.map(const(0))
    wholeTraversal.set(9)(Whole(0)) shouldEqual whole9
  }

  test("over") {
    fromTraversal.over(plusOne)(list) shouldEqual list.map(plusOne)
    wholeTraversal.over(plusOne)(Whole(8)) shouldEqual whole9
  }

  test("traverse") {
    fromTraversal.traverse(list)(_.some) shouldEqual list.some
    fromTraversal.traverse(list)(someEven) shouldEqual None
    fromTraversal.traverse(list)(_.some) shouldEqual fromTraversal.overF(_.some)(list)
    wholeTraversal.traverse(whole9)(_.some) shouldEqual whole9.some
  }

  test("foldMap") {
    fromTraversal.foldMap(list)(_.toString) shouldEqual list.map(_.toString).intercalate("")
    wholeTraversal.foldMap(whole9)(_.toString) shouldEqual 9.toString
  }

  test("fold") {
    fromTraversal.fold(list) shouldEqual list.sum
    fromTraversal.fold(emptyList) shouldEqual 0
    fromTraversal.view(list) shouldEqual fromTraversal.fold(list)
    wholeTraversal.view(whole9) shouldEqual 9
  }

  test("foldRight") {
    fromTraversal.foldRight(list)(emptyList)(_ :: _) shouldEqual list
    wholeTraversal.foldRight(whole9)(0)(_ - _) should be > 0
  }

  test("foldLeft") {
    fromTraversal.foldLeft(list)(List.empty[Int])((ls, a) => a :: ls) shouldEqual list.reverse
    wholeTraversal.foldLeft(whole9)(0)(_ - _) should be < 0
  }

  test("sequence_") {
    fromTraversal.sequence_[Option](list) shouldEqual ().some
    wholeTraversal.sequence_[Option](whole9) shouldEqual ().some
  }

  test("traverse_") {
    fromTraversal.traverse_(list)(_.some) shouldEqual Some(())
    fromTraversal.traverse_(list)(someEven) shouldEqual None
    wholeTraversal.traverse_(whole9)(_.some) shouldEqual Some(())
    wholeTraversal.traverse_(whole9)(someEven) shouldEqual None
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromTraversal.sum(list) shouldEqual list.sum
      wholeTraversal.sum(whole9) shouldEqual 9
    }

    test("product") {
      fromTraversal.product(list) shouldEqual list.product
      fromTraversal.product(emptyList) shouldEqual 1
      wholeTraversal.product(whole9) shouldEqual 9
    }
  }

  test("forall") {
    fromTraversal.forall(_ < 10)(list) shouldEqual true
    fromTraversal.forall(_ < 10)(emptyList) shouldEqual true
    fromTraversal.forall(_ > 10)(list) shouldEqual false
    fromTraversal.forall(_ > 10)(emptyList) shouldEqual true
    wholeTraversal.forall(_ < 10)(whole9) shouldEqual true
    wholeTraversal.forall(_ > 10)(whole9) shouldEqual false
  }

  test("forall using heyting") {
    fromTraversal.forall(list)(_ < 10) shouldEqual true
    fromTraversal.forall(emptyList)(_ < 10) shouldEqual true
    fromTraversal.forall(list)(_ > 10) shouldEqual false
    fromTraversal.forall(emptyList)(_ > 10) shouldEqual true
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
    fromTraversal.any(list)(greaterThan5) shouldEqual true
    fromTraversal.any(emptyList)(greaterThan10) shouldEqual false
    wholeTraversal.any(whole9)(greaterThan5) shouldEqual true
  }

  test("exist") {
    fromTraversal.exists(greaterThan5)(list) shouldEqual true
    fromTraversal.exists(greaterThan10)(list) shouldEqual false
    wholeTraversal.exists(greaterThan5)(whole9) shouldEqual true
    wholeTraversal.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    fromTraversal.notExists(greaterThan5)(list) shouldEqual false
    fromTraversal.notExists(greaterThan10)(list) shouldEqual true
    fromTraversal.notExists(greaterThan10)(list) shouldEqual !fromTraversal.exists(greaterThan10)(list)
    wholeTraversal.notExists(greaterThan5)(whole9) shouldEqual false
    wholeTraversal.notExists(greaterThan10)(whole9) shouldEqual true
    wholeTraversal.notExists(greaterThan10)(whole9) shouldEqual !wholeTraversal.exists(greaterThan10)(whole9)
  }

  test("contains") {
    fromTraversal.contains(5)(list) shouldEqual true
    fromTraversal.contains(10)(list) shouldEqual false
    wholeTraversal.contains(9)(whole9) shouldEqual true
    wholeTraversal.contains(10)(whole9) shouldEqual false
  }

  test("notContains") {
    fromTraversal.notContains(5)(list) shouldEqual false
    fromTraversal.notContains(10)(list) shouldEqual true
    fromTraversal.notContains(10)(list) shouldEqual !fromTraversal.contains(10)(list)
    wholeTraversal.notContains(9)(whole9) shouldEqual false
    wholeTraversal.notContains(10)(whole9) shouldEqual true
    wholeTraversal.notContains(10)(whole9) shouldEqual !wholeTraversal.contains(10)(whole9)
  }

  test("isEmpty") {
    fromTraversal.isEmpty(list) shouldEqual false
    fromTraversal.isEmpty(emptyList) shouldEqual true
    wholeTraversal.isEmpty(whole9) shouldEqual false
  }

  test("nonEmpty") {
    fromTraversal.nonEmpty(list) shouldEqual true
    fromTraversal.nonEmpty(emptyList) shouldEqual false
    fromTraversal.nonEmpty(list) shouldEqual !fromTraversal.isEmpty(list)
    wholeTraversal.nonEmpty(whole9) shouldEqual true
    wholeTraversal.nonEmpty(whole9) shouldEqual !wholeTraversal.isEmpty(whole9)
  }

  test("length") {
    fromTraversal.length(list) shouldEqual list.length
    fromTraversal.length(emptyList) shouldEqual 0
    wholeTraversal.length(whole9) shouldEqual 1
  }

  test("find") {
    fromTraversal.find(greaterThan5)(list) shouldEqual list.find(greaterThan5)
    fromTraversal.find(greaterThan10)(list) shouldEqual None
    wholeTraversal.find(greaterThan5)(whole9) shouldEqual 9.some
    wholeTraversal.find(greaterThan10)(whole9) shouldEqual None
  }

  test("first") {
    fromTraversal.first(list) shouldEqual list.head.some
    fromTraversal.first(emptyList) shouldEqual None
    wholeTraversal.first(whole9) shouldEqual 9.some
  }

  test("last") {
    fromTraversal.last(list) shouldEqual list.last.some
    fromTraversal.last(emptyList) shouldEqual None
    wholeTraversal.last(whole9) shouldEqual 9.some
  }

  test("minimum") {
    fromTraversal.minimum(Random.shuffle(list)) shouldEqual list.head.some
    fromTraversal.minimum(emptyList) shouldEqual None
    wholeTraversal.minimum(whole9) shouldEqual 9.some
  }

  test("maximum") {
    fromTraversal.maximum(Random.shuffle(list)) shouldEqual list.last.some
    fromTraversal.maximum(emptyList) shouldEqual None
    wholeTraversal.maximum(whole9) shouldEqual 9.some
  }

  test("toArray") {
    fromTraversal.toArray(list) shouldEqual list.toArray
    wholeTraversal.toArray(whole9) shouldEqual Array(9)
  }

  test("toList") {
    fromTraversal.toList(list) shouldEqual list
    wholeTraversal.toList(whole9) shouldEqual List(9)
  }

  test("use") {
    implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](1)

    fromTraversal.use.runA(list).value shouldEqual list
    wholeTraversal.use.runA(whole9).value shouldEqual List(9)
  }

  test("compose with Getter") {
    (traversal compose getter).fold(9) shouldEqual 9
  }

  test("compose with Fold") {
    (traversal compose fold).fold(9) shouldEqual 9
  }

  test("asIndexableTraversal") {
    fromTraversal.asIndexableTraversal.foldRight(list)(List.empty[Int])(_._1 :: _) shouldEqual List.range(0, 6)
  }

  test("filterByIndex") {
    fromTraversal.filterByIndex(_ < 3).viewAll(list) shouldEqual list.take(3)
  }

  test("element") {
    fromTraversal.element(1).viewAll(list) shouldEqual List(2)
  }

  test("take") {
    val take3 = Traversal.take[List, Int](3)
    take3.viewAll(list) shouldEqual List(1, 2, 3)
    take3.over(_ + 1)(list) shouldEqual List(2, 3, 4, 4, 5, 6)
  }

  test("drop") {
    val drop3 = Traversal.drop[List, Int](3)
    drop3.viewAll(list) shouldEqual List(4, 5, 6)
    drop3.over(_ + 1)(list) shouldEqual List(1, 2, 3, 5, 6, 7)
  }

  test("takeWhile") {
    val take3 = fromTraversal.takeWhile(_ < 4)
    take3.viewAll(list) shouldEqual List(1, 2, 3)
    take3.over(_ + 1)(list) shouldEqual List(2, 3, 4, 4, 5, 6)
  }

  test("dropWhile") {
    val drop3 = fromTraversal.dropWhile(_ < 4)
    drop3.viewAll(list) shouldEqual List(4, 5, 6)
    drop3.over(_ + 1)(list) shouldEqual List(1, 2, 3, 5, 6, 7)
  }

  test("both") {
    val both = Traversal_.both[Tuple2, String, Int]

    both.viewAll(("Hello", "World!")) shouldEqual List("Hello", "World!")
    both.over(_.length)(("Hello", "World!")) shouldEqual ((5, 6))
    both.foldRight(("Hello ", "World"))("!")(_ ++ _) shouldEqual "Hello World!"
    both.foldLeft(("Hello ", "World!"))("!")(_ ++ _) shouldEqual "!Hello World!"
  }

  test("filter using fold") {
    val filterFold: Fold_[Whole, Whole, Int, Int] =
      Getter[Whole, Int](_.part) compose
        Prism.fromPartial[Int, Int] { case i if i < 5 => i }(identity)
    val traversal = Traversal.fromTraverse[List, Whole] compose Traversal.filter(filterFold)

    traversal.viewAll(List(Whole(1), Whole(9), Whole(2))) shouldEqual List(Whole(1), Whole(2))
  }

  test("filter using traversal") {
    val filterTraversal =
      Lens[Whole, Int](_.part)(const(i => Whole(i))) compose
        Prism.fromPartial[Int, Int] { case i if i < 5 => i }(identity)
    val traversal = Traversal.fromTraverse[List, Whole] compose Traversal.filter(filterTraversal)

    traversal.viewAll(List(Whole(1), Whole(9), Whole(2))) shouldEqual List(Whole(1), Whole(2))
  }
}
