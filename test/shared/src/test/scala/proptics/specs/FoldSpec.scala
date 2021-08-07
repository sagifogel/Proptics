package proptics.specs

import scala.Function.const
import scala.util.Random

import cats.data.State
import cats.syntax.option._
import spire.std.boolean._

import proptics._
import proptics.specs.compose.{getter, _}
import proptics.syntax.fold._

class FoldSpec extends PropticsSuite {
  val ones: List[Int] = List.fill(10)(1)
  val foldable: Fold[Whole, Int] = Fold[Whole, Int](_.part)
  val filtered: Fold[Int, Int] = Fold.filter[Int](evenNumbers)
  val fromFoldable: Fold[List[Int], Int] = Fold.fromFoldable
  val boolFoldable: Fold[List[Boolean], Boolean] = Fold.fromFoldable
  val fromGetter: Fold[List[Int], List[Int]] = Getter[List[Int], List[Int]](identity).asFold

  test("viewAll") {
    fromFoldable.viewAll(list) shouldEqual list
    fromFoldable.viewAll(listEmpty) shouldEqual listEmpty
    foldable.viewAll(whole9) shouldEqual List(whole9.part)
    fromGetter.viewAll(list) shouldEqual List(list)
    fromGetter.viewAll(listEmpty) shouldEqual List(listEmpty)
  }

  test("preview") {
    fromFoldable.preview(list) shouldEqual 1.some
    fromFoldable.preview(listEmpty) shouldEqual None
    foldable.preview(whole9) shouldEqual 9.some
    fromGetter.preview(list) shouldEqual list.some
    fromGetter.preview(listEmpty) shouldEqual listEmpty.some
  }

  test("foldMap") {
    fromFoldable.foldMap(list)(identity) shouldEqual list.sum
    fromFoldable.foldMap(list)(List(_)) shouldEqual list
    fromFoldable.foldMap(listEmpty)(identity) shouldEqual 0
    fromFoldable.foldMap(listEmpty)(List(_)) shouldEqual listEmpty
    foldable.foldMap(whole9)(identity) shouldEqual 9
    fromGetter.foldMap(list)(identity) shouldEqual list
    fromGetter.foldMap(listEmpty)(identity) shouldEqual listEmpty
    fromGetter.foldMap(list)(_.sum) shouldEqual list.sum
  }

  test("fold") {
    fromFoldable.fold(list) shouldEqual list.sum
    fromFoldable.fold(list) shouldEqual fromFoldable.view(list)
    fromFoldable.fold(listEmpty) shouldEqual 0
    fromFoldable.fold(listEmpty) shouldEqual fromFoldable.view(listEmpty)
    foldable.fold(whole9) shouldEqual 9
    foldable.fold(whole9) shouldEqual foldable.view(whole9)
    fromGetter.fold(list) shouldEqual list
  }

  test("foldRight") {
    fromFoldable.foldRight(list)(0)(_ + _) shouldEqual list.sum
    fromFoldable.foldRight(list)(0)(_ + _) should be > 0
    fromFoldable.foldRight(list)(listEmpty)(_ :: _) shouldEqual list
    fromFoldable.foldRight(listEmpty)(0)(_ + _) shouldEqual 0
    fromFoldable.foldRight(listEmpty)(0)(_ - _) shouldEqual 0
    foldable.foldRight(whole9)(1)(_ + _) shouldEqual 10
    foldable.foldRight(whole9)(1)(_ - _) shouldEqual 8
    fromGetter.foldRight(list)(0)(_.sum + _) shouldEqual list.sum
  }

  test("foldLeft") {
    fromFoldable.foldLeft(list)(0)(_ + _) shouldEqual list.sum
    fromFoldable.foldLeft(list)(0)(_ + _) should be > 0
    fromFoldable.foldLeft(list)(listEmpty)((ls, a) => a :: ls) shouldEqual list.reverse
    fromFoldable.foldLeft(listEmpty)(0)(_ + _) shouldEqual 0
    fromFoldable.foldLeft(listEmpty)(0)(_ - _) shouldEqual 0
    foldable.foldLeft(whole9)(1)(_ + _) shouldEqual 10
    foldable.foldLeft(whole9)(1)(_ - _) shouldEqual -8
    fromGetter.foldLeft(list)(0)(_ + _.sum) shouldEqual list.sum
  }

  {
    import spire.std.int.IntAlgebra

    test("sum") {
      fromFoldable.sum(list) shouldEqual list.sum
      foldable.sum(whole9) shouldEqual 9
    }

    test("product") {
      fromFoldable.product(list) shouldEqual list.product
      fromFoldable.product(listEmpty) shouldEqual 1
      foldable.product(whole9) shouldEqual 9
    }
  }

  test("forall") {
    fromFoldable.forall(_ < 10)(list) shouldEqual true
    fromFoldable.forall(_ < 10)(listEmpty) shouldEqual true
    fromFoldable.forall(_ > 10)(list) shouldEqual false
    fromFoldable.forall(_ > 10)(listEmpty) shouldEqual true
    foldable.forall(_ < 10)(whole9) shouldEqual true
    foldable.forall(_ > 10)(whole9) shouldEqual false
    fromGetter.forall(_.forall(_ < 10))(list) shouldEqual true
    fromGetter.forall(_.forall(_ < 10))(listEmpty) shouldEqual true
    fromGetter.forall(_.forall(_ > 10))(list) shouldEqual false
  }

  test("forall using heyting") {
    fromFoldable.forall(list)(_ < 10) shouldEqual true
    fromFoldable.forall(listEmpty)(_ < 10) shouldEqual true
    fromFoldable.forall(list)(_ > 10) shouldEqual false
    fromFoldable.forall(listEmpty)(_ > 10) shouldEqual true
    foldable.forall(whole9)(_ < 10) shouldEqual true
    foldable.forall(whole9)(_ > 10) shouldEqual false
    fromGetter.forall(list)(_.forall(_ < 10)) shouldEqual true
    fromGetter.forall(listEmpty)(_.forall(_ < 10)) shouldEqual true
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
    fromFoldable.any(listEmpty)(greaterThan10) shouldEqual false
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
    fromGetter.notContains(list)(listEmpty) shouldEqual true
    fromGetter.notContains(list)(list) shouldEqual false
    fromGetter.notContains(list)(list) shouldEqual !fromGetter.contains(list)(list)
  }

  test("isEmpty") {
    fromFoldable.isEmpty(list) shouldEqual false
    fromFoldable.isEmpty(listEmpty) shouldEqual true
    foldable.isEmpty(whole9) shouldEqual false
    fromGetter.isEmpty(list) shouldEqual false
    fromGetter.isEmpty(listEmpty) shouldEqual false
  }

  test("nonEmpty") {
    fromFoldable.nonEmpty(list) shouldEqual true
    fromFoldable.nonEmpty(listEmpty) shouldEqual false
    fromFoldable.nonEmpty(list) shouldEqual !fromFoldable.isEmpty(list)
    foldable.nonEmpty(whole9) shouldEqual true
    foldable.nonEmpty(whole9) shouldEqual !foldable.isEmpty(whole9)
    fromGetter.nonEmpty(list) shouldEqual true
    fromGetter.nonEmpty(listEmpty) shouldEqual true
  }

  test("length") {
    fromFoldable.length(list) shouldEqual list.length
    fromFoldable.length(listEmpty) shouldEqual 0
    foldable.length(whole9) shouldEqual 1
    fromGetter.length(list) shouldEqual 1
  }

  test("find") {
    fromFoldable.find(greaterThan5)(list) shouldEqual list.find(greaterThan5)
    fromFoldable.find(greaterThan10)(list) shouldEqual None
    foldable.find(greaterThan5)(whole9) shouldEqual 9.some
    foldable.find(greaterThan10)(whole9) shouldEqual None
    fromGetter.find(_.exists(greaterThan5))(list) shouldEqual list.some
    fromGetter.find(_.exists(greaterThan10))(list) shouldEqual None
  }

  test("first") {
    fromFoldable.first(list) shouldEqual list.head.some
    fromFoldable.first(listEmpty) shouldEqual None
    foldable.first(whole9) shouldEqual 9.some
    fromGetter.first(list) shouldEqual list.some
  }

  test("last") {
    fromFoldable.last(list) shouldEqual list.last.some
    fromFoldable.last(listEmpty) shouldEqual None
    foldable.last(whole9) shouldEqual 9.some
    fromGetter.last(list) shouldEqual list.some
  }

  test("minimum") {
    fromFoldable.minimum(Random.shuffle(list)) shouldEqual list.head.some
    fromFoldable.minimum(listEmpty) shouldEqual None
    foldable.minimum(whole9) shouldEqual 9.some
  }

  test("maximum") {
    fromFoldable.maximum(Random.shuffle(list)) shouldEqual list.last.some
    fromFoldable.maximum(listEmpty) shouldEqual None
    foldable.maximum(whole9) shouldEqual 9.some
  }

  test("toArray") {
    fromFoldable.toArray(list) shouldEqual list.toArray
    foldable.toArray(whole9) shouldEqual Array(9)
    fromGetter.toArray(list) shouldEqual Array(list)
  }

  test("toList") {
    fromFoldable.toList(list) shouldEqual list
    foldable.toList(whole9) shouldEqual List(9)
    fromGetter.toList(list) shouldEqual List(list)
  }

  test("use") {
    implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](1)

    fromFoldable.use.runA(list).value shouldEqual list
    foldable.use.runA(whole9).value shouldEqual List(9)
  }

  test("to") {
    fold.to(_ + 1).fold(8) shouldEqual 9
  }

  test("compose with Iso") {
    (fold compose iso).fold(9) shouldEqual 9
  }

  test("andThen with Iso") {
    (fold andThen iso).fold(9) shouldEqual 9
  }

  test("compose with AnIso") {
    (fold compose anIso).fold(9) shouldEqual 9
  }

  test("andThen with AnIso") {
    (fold andThen anIso).fold(9) shouldEqual 9
  }

  test("compose with Lens") {
    (fold compose lens).fold(9) shouldEqual 9
  }

  test("andThen with Lens") {
    (fold andThen lens).fold(9) shouldEqual 9
  }

  test("compose with ALens") {
    (fold compose aLens).fold(9) shouldEqual 9
  }

  test("andThen with ALens") {
    (fold andThen aLens).fold(9) shouldEqual 9
  }

  test("compose with Prism") {
    (fold compose prism).fold(9) shouldEqual 9
  }

  test("andThen with Prism") {
    (fold andThen prism).fold(9) shouldEqual 9
  }

  test("compose with APrism") {
    (fold compose aPrism).fold(9) shouldEqual 9
  }

  test("andThen with APrism") {
    (fold andThen aPrism).fold(9) shouldEqual 9
  }

  test("compose with AffineTraversal") {
    (fold compose affineTraversal).fold(9) shouldEqual 9
  }

  test("andThen with AffineTraversal") {
    (fold andThen affineTraversal).fold(9) shouldEqual 9
  }

  test("compose with AnAffineTraversal") {
    (fold compose anAffineTraversal).fold(9) shouldEqual 9
  }

  test("andThen with AnAffineTraversal") {
    (fold andThen anAffineTraversal).fold(9) shouldEqual 9
  }

  test("compose with Traversal") {
    (fold compose traversal).fold(9) shouldEqual 9
  }

  test("andThen with Traversal") {
    (fold andThen traversal).fold(9) shouldEqual 9
  }

  test("compose with ATraversal") {
    (fold compose aTraversal).fold(9) shouldEqual 9
  }

  test("andThen with ATraversal") {
    (fold andThen aTraversal).fold(9) shouldEqual 9
  }

  test("compose with Getter") {
    (fold compose getter).fold(9) shouldEqual 9
  }

  test("andThen with Getter") {
    (fold andThen getter).fold(9) shouldEqual 9
  }

  test("compose with Fold") {
    (fold compose fold).fold(9) shouldEqual 9
  }

  test("andThen with Fold") {
    (fold andThen fold).fold(9) shouldEqual 9
  }

  test("compose with IndexedLens") {
    val composed = Fold[List[Int], List[Int]](identity) compose
      IndexedLens[Int, List[Int], List[Int]](ls => (ls.take(1), 0))(ls1 => ls2 => ls2.take(1) ++ ls1.drop(1))

    composed.foldMap(list)(_._1) shouldEqual List(1)
  }

  test("andThen with IndexedLens") {
    val composed = Fold[List[Int], List[Int]](identity) andThen
      IndexedLens[Int, List[Int], List[Int]](ls => (ls.take(1), 0))(ls1 => ls2 => ls2.take(1) ++ ls1.drop(1))

    composed.foldMap(list)(_._1) shouldEqual List(1)
  }

  test("compose with AnIndexedLens") {
    val composed = Fold[List[Int], List[Int]](identity) compose
      AnIndexedLens[Int, List[Int], List[Int]](ls => (ls.take(1), 0))(ls1 => ls2 => ls2.take(1) ++ ls1.drop(1))

    composed.foldMap(list)(_._1) shouldEqual List(1)
  }

  test("andThen with AnIndexedLens") {
    val composed = Fold[List[Int], List[Int]](identity) andThen
      AnIndexedLens[Int, List[Int], List[Int]](ls => (ls.take(1), 0))(ls1 => ls2 => ls2.take(1) ++ ls1.drop(1))

    composed.foldMap(list)(_._1) shouldEqual List(1)
  }

  test("compose with IndexedTraversal") {
    (Fold[List[Int], List[Int]](identity) compose
      IndexedTraversal.fromTraverse[List, Int]).foldMap(list) { case (_, i) => List(i) } shouldEqual list.zipWithIndex.map(_._2)
  }

  test("andThen with IndexedTraversal") {
    (Fold[List[Int], List[Int]](identity) andThen
      IndexedTraversal.fromTraverse[List, List[Int]]).foldMap(list.map(List(_))) { case (_, i) => List(i) } shouldEqual list.zipWithIndex.map(_._2)
  }

  test("compose with IndexedGetter") {
    val composed = fold compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter") {
    val composed = fold andThen indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = fold compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedFold") {
    val composed = fold andThen indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("asIndexableTraversal") {
    fromFoldable.asIndexableFold.foldRight(list)(List.empty[Int])(_._2 :: _) shouldEqual List.range(0, 6)
  }

  test("filterByIndex") {
    fromFoldable.filterByIndex(_ < 3).viewAll(list) shouldEqual list.take(3)
  }

  test("element") {
    fromFoldable.elementAt(1).viewAll(list) shouldEqual List(2)
  }

  test("take") {
    val take3 = Fold.take[List, Int](3)
    take3.viewAll(list) shouldEqual List(1, 2, 3)
  }

  test("drop") {
    val drop3 = Fold.drop[List, Int](3)
    drop3.viewAll(list) shouldEqual List(4, 5, 6)
  }

  test("takeWhile") {
    val take3 = fromFoldable.takeWhile(_ < 4)
    take3.viewAll(list) shouldEqual List(1, 2, 3)
  }

  test("dropWhile") {
    val drop3 = fromFoldable.dropWhile(_ < 4)
    drop3.viewAll(list) shouldEqual List(4, 5, 6)
  }

  test("both") {
    val both = Fold_.both[Tuple2, String, String]

    both.viewAll(("Hello", "World!")) shouldEqual List("Hello", "World!")
    both.foldRight(("Hello ", "World"))("!")(_ ++ _) shouldEqual "Hello World!"
    both.foldLeft(("Hello ", "World!"))("!")(_ ++ _) shouldEqual "!Hello World!"
  }

  test("filter using fold") {
    val traversal =
      Getter[Whole, Int](_.part) compose
        Prism.fromPartial[Int, Int] { case i if i < 5 => i }(identity)
    val fold = Fold.fromFoldable[List, Whole] compose Fold.filter(traversal)

    fold.viewAll(List(Whole(1), Whole(9), Whole(2))) shouldEqual List(Whole(1), Whole(2))
  }

  test("filter using traversal") {
    val traversal =
      Lens[Whole, Int](_.part)(const(i => Whole(i))) compose
        Prism.fromPartial[Int, Int] { case i if i < 5 => i }(identity)
    val fold = Fold.fromFoldable[List, Whole] compose Fold.filter(traversal)

    fold.viewAll(List(Whole(1), Whole(9), Whole(2))) shouldEqual List(Whole(1), Whole(2))
  }

  test("implicit cast to from Lens") {
    val foldFromLens: Fold[Int, Int] = lens

    foldFromLens.foldMap(8)(_ + 1) shouldEqual 9
  }

  test("implicit cast to from ALens") {
    val foldFromALens: Fold[Int, Int] = aLens

    foldFromALens.foldMap(8)(_ + 1) shouldEqual 9
  }

  test("implicit cast to from Prism") {
    val foldFromPrism: Fold[Int, Int] = prism

    foldFromPrism.foldMap(8)(_ + 1) shouldEqual 9
  }

  test("implicit cast to from APrism") {
    val foldFromAPrism: Fold[Int, Int] = aPrism

    foldFromAPrism.foldMap(8)(_ + 1) shouldEqual 9
  }

  test("implicit cast to from AffineTraversal") {
    val foldFromAffineTraversal: Fold[Int, Int] = affineTraversal

    foldFromAffineTraversal.foldMap(8)(_ + 1) shouldEqual 9
  }

  test("implicit cast to from AnAffineTraversal") {
    val foldFromAnAffineTraversal: Fold[Int, Int] = anAffineTraversal

    foldFromAnAffineTraversal.foldMap(8)(_ + 1) shouldEqual 9
  }

  test("implicit cast to from Traversal") {
    val foldFromTraversal: Fold[Int, Int] = traversal

    foldFromTraversal.foldMap(8)(_ + 1) shouldEqual 9
  }

  test("implicit cast to from ATraversal") {
    val foldFromLATraversal: Fold[Int, Int] = aTraversal

    foldFromLATraversal.foldMap(8)(_ + 1) shouldEqual 9
  }

  test("implicit cast to from Getter") {
    val foldFromLGetter: Fold[Int, Int] = getter

    foldFromLGetter.foldMap(8)(_ + 1) shouldEqual 9
  }
}
