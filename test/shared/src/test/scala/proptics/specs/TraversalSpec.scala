package proptics.specs

import scala.Function.const
import scala.util.Random

import cats.data.State
import cats.instances.list._
import cats.kernel.Order
import cats.syntax.foldable._
import cats.syntax.option._
import spire.std.boolean._

import proptics._
import proptics.law.discipline._
import proptics.specs.compose._
import proptics.syntax.traversal._

class TraversalSpec extends PropticsSuite {
  val plusOne: Int => Int = _ + 1
  val someEven: Int => Option[Int] = i => if (i % 2 == 0) i.some else none[Int]
  val fromTraverse: Traversal[List[Int], Int] = Traversal.fromTraverse[List, Int]
  val boolTraversal: Traversal[List[Boolean], Boolean] = Traversal.fromTraverse[List, Boolean]
  val wholeTraversal: Traversal[Whole, Int] = Traversal[Whole, Int](_.part)(whole => focus => whole.copy(part = focus))

  checkAll("Traversal[List[Int], Int] fromTraverse", TraversalTests(fromTraverse).traversal)
  checkAll("Traversal[Whole, Int] apply", TraversalTests(wholeTraversal).traversal)
  checkAll("Traversal[Whole, Int] asATraversal", ATraversalTests(wholeTraversal.asATraversal).aTraversal)
  checkAll("Traversal[Int, Int] id", TraversalTests(Traversal.id[Int]).traversal)
  checkAll("Traversal[(Int, Int), (Int, Int), Int, Int] both", TraversalTests(Traversal_.both[(*, *), Int, Int]).traversal)
  checkAll("Traversal[List[Int], Int] elementAt", TraversalTests(Traversal.elementAt[List, Int](1)).traversal)
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

  {
    implicit val order: Order[List[Int]] = catsKernelStdOrderForList[Int]

    checkAll("Traversal[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedTraversalTests(traversal compose indexedLens).indexedTraversal)
    checkAll("Traversal[Int, Int] compose with AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(traversal compose anIndexedLens).indexedTraversal)
    checkAll("Traversal[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(traversal compose indexedTraversal).indexedTraversal)
    checkAll("Traversal[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(traversal compose indexedSetter).indexedSetter)
  }

  test("viewAll") {
    fromTraverse.viewAll(list) shouldEqual list
    fromTraverse.viewAll(listEmpty) shouldEqual listEmpty
    wholeTraversal.viewAll(whole9) shouldEqual List(9)
  }

  test("preview") {
    fromTraverse.preview(list) shouldEqual Some(1)
    fromTraverse.preview(listEmpty) shouldEqual None
    wholeTraversal.preview(whole9) shouldEqual Some(9)
  }

  test("set") {
    fromTraverse.set(0)(list) shouldEqual list.map(const(0))
    wholeTraversal.set(9)(Whole(0)) shouldEqual whole9
  }

  test("over") {
    fromTraverse.over(plusOne)(list) shouldEqual list.map(plusOne)
    wholeTraversal.over(plusOne)(Whole(8)) shouldEqual whole9
  }

  test("traverse") {
    fromTraverse.traverse(list)(_.some) shouldEqual list.some
    fromTraverse.traverse(list)(someEven) shouldEqual None
    fromTraverse.traverse(list)(_.some) shouldEqual fromTraverse.overF(_.some)(list)
    wholeTraversal.traverse(whole9)(_.some) shouldEqual whole9.some
  }

  test("foldMap") {
    fromTraverse.foldMap(list)(_.toString) shouldEqual list.map(_.toString).intercalate("")
    wholeTraversal.foldMap(whole9)(_.toString) shouldEqual 9.toString
  }

  test("fold") {
    fromTraverse.fold(list) shouldEqual list.sum
    fromTraverse.fold(listEmpty) shouldEqual 0
    fromTraverse.view(list) shouldEqual fromTraverse.fold(list)
    wholeTraversal.view(whole9) shouldEqual 9
  }

  test("foldRight") {
    fromTraverse.foldRight(list)(listEmpty)(_ :: _) shouldEqual list
    wholeTraversal.foldRight(whole9)(0)(_ - _) should be > 0
  }

  test("foldLeft") {
    fromTraverse.foldLeft(list)(listEmpty)((ls, a) => a :: ls) shouldEqual list.reverse
    wholeTraversal.foldLeft(whole9)(0)(_ - _) should be < 0
  }

  test("sequence_") {
    fromTraverse.sequence_[Option](list) shouldEqual ().some
    wholeTraversal.sequence_[Option](whole9) shouldEqual ().some
  }

  test("traverse_") {
    fromTraverse.traverse_(list)(_.some) shouldEqual Some(())
    fromTraverse.traverse_(list)(someEven) shouldEqual None
    wholeTraversal.traverse_(whole9)(_.some) shouldEqual Some(())
    wholeTraversal.traverse_(whole9)(someEven) shouldEqual None
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromTraverse.sum(list) shouldEqual list.sum
      wholeTraversal.sum(whole9) shouldEqual 9
    }

    test("product") {
      fromTraverse.product(list) shouldEqual list.product
      fromTraverse.product(listEmpty) shouldEqual 1
      wholeTraversal.product(whole9) shouldEqual 9
    }
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

  test("isEmpty") {
    fromTraverse.isEmpty(list) shouldEqual false
    fromTraverse.isEmpty(listEmpty) shouldEqual true
    wholeTraversal.isEmpty(whole9) shouldEqual false
  }

  test("nonEmpty") {
    fromTraverse.nonEmpty(list) shouldEqual true
    fromTraverse.nonEmpty(listEmpty) shouldEqual false
    fromTraverse.nonEmpty(list) shouldEqual !fromTraverse.isEmpty(list)
    wholeTraversal.nonEmpty(whole9) shouldEqual true
    wholeTraversal.nonEmpty(whole9) shouldEqual !wholeTraversal.isEmpty(whole9)
  }

  test("length") {
    fromTraverse.length(list) shouldEqual list.length
    fromTraverse.length(listEmpty) shouldEqual 0
    wholeTraversal.length(whole9) shouldEqual 1
  }

  test("find") {
    fromTraverse.find(greaterThan5)(list) shouldEqual list.find(greaterThan5)
    fromTraverse.find(greaterThan10)(list) shouldEqual None
    wholeTraversal.find(greaterThan5)(whole9) shouldEqual 9.some
    wholeTraversal.find(greaterThan10)(whole9) shouldEqual None
  }

  test("first") {
    fromTraverse.first(list) shouldEqual list.head.some
    fromTraverse.first(listEmpty) shouldEqual None
    wholeTraversal.first(whole9) shouldEqual 9.some
  }

  test("last") {
    fromTraverse.last(list) shouldEqual list.last.some
    fromTraverse.last(listEmpty) shouldEqual None
    wholeTraversal.last(whole9) shouldEqual 9.some
  }

  test("minimum") {
    fromTraverse.minimum(Random.shuffle(list)) shouldEqual list.head.some
    fromTraverse.minimum(listEmpty) shouldEqual None
    wholeTraversal.minimum(whole9) shouldEqual 9.some
  }

  test("maximum") {
    fromTraverse.maximum(Random.shuffle(list)) shouldEqual list.last.some
    fromTraverse.maximum(listEmpty) shouldEqual None
    wholeTraversal.maximum(whole9) shouldEqual 9.some
  }

  test("toArray") {
    fromTraverse.toArray(list) shouldEqual list.toArray
    wholeTraversal.toArray(whole9) shouldEqual Array(9)
  }

  test("toList") {
    fromTraverse.toList(list) shouldEqual list
    wholeTraversal.toList(whole9) shouldEqual List(9)
  }

  test("use") {
    implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](1)

    fromTraverse.use.runA(list).value shouldEqual list
    wholeTraversal.use.runA(whole9).value shouldEqual List(9)
  }

  test("compose with Getter") {
    (traversal compose getter).fold(9) shouldEqual 9
  }

  test("compose with Fold") {
    (traversal compose fold).fold(9) shouldEqual 9
  }

  test("asIndexableTraversal") {
    fromTraverse.asIndexableTraversal.foldRight(list)(List.empty[Int])(_._2 :: _) shouldEqual List.range(0, 6)
  }

  test("filterByIndex") {
    fromTraverse.filterByIndex(_ < 3).viewAll(list) shouldEqual list.take(3)
  }

  test("element") {
    fromTraverse.elementAt(1).viewAll(list) shouldEqual List(2)
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
    val take3 = fromTraverse.takeWhile(_ < 4)
    take3.viewAll(list) shouldEqual List(1, 2, 3)
    take3.over(_ + 1)(list) shouldEqual List(2, 3, 4, 4, 5, 6)
  }

  test("dropWhile") {
    val drop3 = fromTraverse.dropWhile(_ < 4)
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
    val traversal = Traversal.fromTraverse[List, Whole] compose Traversal.filter(filterTraversal.asFold)

    traversal.viewAll(List(Whole(1), Whole(9), Whole(2))) shouldEqual List(Whole(1), Whole(2))
  }

  test("compose with IndexedGetter") {
    val composed = traversal compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = traversal compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
