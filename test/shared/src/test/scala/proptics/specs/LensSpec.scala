package proptics.specs

import cats.instances.option.catsStdInstancesForOption
import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.syntax.option._
import cats.{Eq, Id}
import org.scalacheck.Arbitrary._

import proptics.instances.fields._
import proptics.instances.partsOf._
import proptics.law.discipline._
import proptics.macros.GLens
import proptics.specs.compose._
import proptics.std.tuple._
import proptics.syntax.aTraversal._
import proptics.syntax.traversal._
import proptics.{ATraversal, ATraversal_, Getter, IndexedGetter, Iso, Lens, Lens_, Prism, Traversal, Traversal_}

class LensSpec extends PropticsSuite {
  val wholeLens: Lens[Whole, Int] = Lens[Whole, Int](_.part)(w => i => w.copy(part = i))
  val traversedTuple1: Traversal[List[(String, Int)], String] = Traversal.fromTraverse[List, (String, Int)] compose _1[String, Int]
  val aTraversedTuple1: ATraversal[List[(String, Int)], String] = ATraversal.fromTraverse[List, (String, Int)] compose _1[String, Int]
  val traversedTuple2: Traversal[List[(String, Double)], Double] = Traversal.fromTraverse[List, (String, Double)] compose _2[String, Double]
  val traversedTuple3: Traversal_[List[(String, Int)], List[(Boolean, Int)], String, Boolean] =
    Traversal_.fromTraverse[List, (String, Int), (Boolean, Int)] compose _1P[String, Boolean, Int]
  val aTraversedTuple2: ATraversal[List[(String, Double)], Double] = ATraversal.fromTraverse[List, (String, Double)] compose _2[String, Double]
  val aTraversedTuple3: ATraversal_[List[(String, Int)], List[(Boolean, Int)], String, Boolean] =
    ATraversal_.fromTraverse[List, (String, Int), (Boolean, Int)] compose _1P[String, Boolean, Int]
  val partsOfFromTraversal1: Lens[List[(String, Int)], List[String]] = traversedTuple1.partsOf
  val partsOfFromATraversal1: Lens[List[(String, Int)], List[String]] = aTraversedTuple1.partsOf
  val partsOfFromTraversal2: Lens[List[(String, Double)], List[Double]] = traversedTuple2.partsOf
  val partsOfFromATraversal2: Lens[List[(String, Double)], List[Double]] = aTraversedTuple2.partsOf
  val unsafePartsOfFromTraversal: Lens_[List[(String, Int)], List[(Boolean, Int)], List[String], List[Boolean]] = traversedTuple3.unsafePartsOf
  val unsafePartsOfFromATraversal: Lens_[List[(String, Int)], List[(Boolean, Int)], List[String], List[Boolean]] = aTraversedTuple3.unsafePartsOf
  val firstLevelGLens: Lens[Person, String] = GLens[Person](_.name)
  val leafLevelGLens: Lens[Person, Int] = GLens[Person](_.address.street.number)
  implicit def eqArrow(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => Int] = Eq.instance[Int => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int) === f2(int)
    }
  }

  checkAll("GLens[Person, String] top level gen", LensTests(firstLevelGLens).lens)
  checkAll("GLens[Person, Int] leaf level gen", LensTests(leafLevelGLens).lens)
  checkAll("Lens[Int, Int] id", LensTests(Lens.id[Int]).lens)
  checkAll("Lens[Whole, Int] apply", LensTests(wholeLens).lens)
  checkAll("Lens[(Int, Int), Int] first", Field1Tests[Int, Int].first)
  checkAll("Lens[(Int, Int), Int] second", Field2Tests[Int, Int].second)
  checkAll("Lens[(Int, String), Int] _1", LensTests(_1[Int, String]).lens)
  checkAll("Lens[(Int, Int), Int] third", Field3Tests[Int, Int, Int].third)
  checkAll("Lens[(Int, String), String] _2", LensTests(_2[Int, String]).lens)
  checkAll("Lens[(Int, Int), Int] fourth", Field4Tests[Int, Int, Int, Int].fourth)
  checkAll("Lens[(Int, Int), Int] fifth", Field5Tests[Int, Int, Int, Int, Int].fifth)
  checkAll("Lens[Int, Int] compose with Iso[Int, Int]", LensTests(lens compose iso).lens)
  checkAll("Lens[Int, Int] andThen with Iso[Int, Int]", LensTests(lens andThen iso).lens)
  checkAll("Lens[Int, Int] compose with Lens[Int, Int]", LensTests(lens compose lens).lens)
  checkAll("Lens[Int, Int] andThen with Lens[Int, Int]", LensTests(lens andThen lens).lens)
  checkAll("Lens[(Int, Int), (Int, Int), Int, Int] _1P", LensTests(_1P[Int, Int, Int]).lens)
  checkAll("Lens[(Int, Int), (Int, Int), Int, Int] _2P", LensTests(_2P[Int, Int, Int]).lens)
  checkAll("Lens[Int, Int] compose with AnIso[Int, Int]", LensTests(lens compose anIso).lens)
  checkAll("Lens[Int, Int] andThen with AnIso[Int, Int]", LensTests(lens andThen anIso).lens)
  checkAll("Lens[Int, Int] compose with ALens[Int, Int]", ALensTests(lens compose aLens).aLens)
  checkAll("Lens[Int, Int] andThen with ALens[Int, Int]", ALensTests(lens andThen aLens).aLens)
  checkAll("Lens[Int => Int, Int => Int] outside", LensTests(Lens.outside[Int, Int, Int](Prism.id[Int])).lens)
  checkAll("Lens[Int, Int] compose with Prism[Int, Int]", AffineTraversalTests(lens compose prism).affineTraversal)
  checkAll("Lens[Int, Int] andThen with Prism[Int, Int]", AffineTraversalTests(lens andThen prism).affineTraversal)
  checkAll("Lens[Int, Int] compose with APrism[Int, Int]", AffineTraversalTests(lens compose aPrism).affineTraversal)
  checkAll("Lens[Int, Int] andThen with APrism[Int, Int]", AffineTraversalTests(lens andThen aPrism).affineTraversal)
  checkAll("Lens[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(lens compose affineTraversal).affineTraversal)
  checkAll("Lens[Int, Int] andThen with AffineTraversal[Int, Int]", AffineTraversalTests(lens andThen affineTraversal).affineTraversal)
  checkAll("Lens[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(lens compose anAffineTraversal).anAffineTraversal)
  checkAll("Lens[Int, Int] andThen with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(lens andThen anAffineTraversal).anAffineTraversal)
  checkAll("Lens[Int, Int] compose with Traversal[Int, Int]", TraversalTests(lens compose traversal).traversal)
  checkAll("Lens[Int, Int] andThen with Traversal[Int, Int]", TraversalTests(lens andThen traversal).traversal)
  checkAll("Lens[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(lens compose aTraversal).aTraversal)
  checkAll("Lens[Int, Int] andThen with ATraversal[Int, Int]", ATraversalTests(lens andThen aTraversal).aTraversal)
  checkAll("Lens[Int, Int] compose with Setter[Int, Int]", SetterTests(lens compose setter).setter)
  checkAll("Lens[Int, Int] andThen with Setter[Int, Int]", SetterTests(lens andThen setter).setter)
  checkAll("Lens[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedLensTests(lens compose indexedLens).indexedLens)
  checkAll("Lens[Int, Int] andThen with IndexedLens[Int, Int, Int]", IndexedLensTests(lens andThen indexedLens).indexedLens)
  checkAll("Lens[Int, Int] compose with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(lens compose anIndexedLens).anIndexedLens)
  checkAll("Lens[Int, Int] andThen with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(lens andThen anIndexedLens).anIndexedLens)
  checkAll("Lens[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(lens compose indexedTraversal).indexedTraversal)
  checkAll("Lens[Int, Int] andThen with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(lens andThen indexedTraversal).indexedTraversal)
  checkAll("Lens[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(lens compose indexedSetter).indexedSetter)
  checkAll("Lens[Int, Int] andThen with IndexedSetter[Int, Int, Int]", IndexedSetterTests(lens andThen indexedSetter).indexedSetter)

  test("view") {
    wholeLens.view(whole9) shouldEqual 9
  }

  test("set") {
    wholeLens.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    wholeLens.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
  test("traverse") {
    wholeLens.traverse(whole9)(_.some) shouldEqual Some(whole9)
    wholeLens.traverse(whole9)(_.some) shouldEqual wholeLens.overF(_.some)(whole9)
  }

  test("find") {
    wholeLens.find(greaterThan5)(whole9) shouldEqual Some(9)
    wholeLens.find(greaterThan10)(whole9) shouldEqual None
  }

  test("exists") {
    wholeLens.exists(greaterThan5)(whole9) shouldEqual true
    wholeLens.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    wholeLens.notExists(greaterThan10)(whole9) shouldEqual true
    wholeLens.notExists(greaterThan5)(whole9) shouldEqual false
    wholeLens.notExists(greaterThan5)(whole9) shouldEqual (!wholeLens.exists(greaterThan5)(whole9))
  }

  test("contains") {
    wholeLens.contains(9)(whole9) shouldEqual true
    wholeLens.contains(5)(whole9) shouldEqual false
  }

  test("notContains") {
    wholeLens.notContains(5)(whole9) shouldEqual true
    wholeLens.notContains(9)(whole9) shouldEqual false
    wholeLens.notContains(9)(whole9) shouldEqual (!wholeLens.contains(9)(whole9))
  }

  test("use") {
    wholeLens.use.runA(whole9).value shouldEqual 9
  }

  test("failover") {
    val res = wholeLens.failover[Option](identity)(whole9)(strongStarTupleOfDisj, catsStdInstancesForOption)
    val negativeRes = wholeLens.failover[Option](identity)(whole9)(strongStarTupleOfNegativeDisj, catsStdInstancesForOption)

    res shouldEqual Some(whole9)
    negativeRes shouldEqual None
  }

  test("zipWith") {
    wholeLens.zipWith(Whole(8), Whole(1))(_ + _) shouldEqual whole9
  }

  test("cotraverse") {
    val cotraversedWhole = wholeLens.cotraverse[Id](whole9)(identity)

    cotraversedWhole shouldEqual whole9
    wholeLens.zipWithF[Id](identity)(whole9) shouldEqual cotraversedWhole
  }

  test("compose with Getter") {
    (lens compose getter).view(9) shouldEqual 9
  }

  test("andThen with Getter") {
    (iso andThen getter).view(9) shouldEqual 9
    (iso andThen Getter[Int, Int](_ + 1)).view(8) shouldEqual 9
  }

  test("compose with Fold") {
    (lens compose fold).fold(9) shouldEqual 9
  }

  test("andThen with Fold") {
    (iso andThen fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (iso compose review).review(9) shouldEqual 9
  }

  test("andThen with review") {
    (iso andThen review).review(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = lens compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter[Int, Int, Int]") {
    val composed = Iso.id[Int] andThen IndexedGetter[Int, Int, Int]((_, 1))

    composed.view(9) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = lens compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedFold") {
    val composed = iso andThen indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("partsOf from Traversal can view focuses as List") {
    val target = List("A", "B", "C").zipWithIndex
    partsOfFromTraversal1.view(target) shouldEqual List("A", "B", "C")
  }

  test("partsOf from ATraversal can view focuses as List") {
    val target = List("A", "B", "C").zipWithIndex
    partsOfFromATraversal1.view(target) shouldEqual List("A", "B", "C")
  }

  test("partsOf from Traversal can set the lens to a list to replace the corresponding elements") {
    val target = List("A", "B", "C").zipWithIndex
    partsOfFromTraversal1.set(List("C", "A", "T"))(target) shouldEqual List(("C", 0), ("A", 1), ("T", 2))
  }

  test("partsOf from ATraversal can set the lens to a list to replace the corresponding elements") {
    val target = List("A", "B", "C").zipWithIndex
    partsOfFromATraversal1.set(List("C", "A", "T"))(target) shouldEqual List(("C", 0), ("A", 1), ("T", 2))
  }

  test("partsOf from Traversal ignores extra elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List("C", "A", "T", "E", "G", "O", "R", "Y")
    partsOfFromTraversal1.set(replaceList)(target) shouldEqual List(("C", 0), ("A", 1), ("T", 2))
  }

  test("partsOf from ATraversal ignores extra elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List("C", "A", "T", "E", "G", "O", "R", "Y")
    partsOfFromATraversal1.set(replaceList)(target) shouldEqual List(("C", 0), ("A", 1), ("T", 2))
  }

  test("partsOf from Traversal keeps the originals in case provided with few elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List("C")
    partsOfFromTraversal1.set(replaceList)(target) shouldEqual List(("C", 0), ("B", 1), ("C", 2))
  }

  test("partsOf from ATraversal keeps the originals in case provided with few elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List("C")
    partsOfFromATraversal1.set(replaceList)(target) shouldEqual List(("C", 0), ("B", 1), ("C", 2))
  }

  test("partsOf from Traversal can edit elements using their neighbours as context") {
    val target = List("A", "B", "C").zipWithIndex map { case (s, i) => (s, i + 1 / 1.0) }
    partsOfFromTraversal2.over { ls =>
      val sum = ls.sum
      ls.map(_ / sum)
    }(target) shouldEqual List(("A", 0.16666666666666666), ("B", 0.3333333333333333), ("C", 0.5))
  }

  test("partsOf from ATraversal can edit elements using their neighbours as context") {
    val target = List("A", "B", "C").zipWithIndex map { case (s, i) => (s, i + 1 / 1.0) }
    partsOfFromATraversal2.over { ls =>
      val sum = ls.sum
      ls.map(_ / sum)
    }(target) shouldEqual List(("A", 0.16666666666666666), ("B", 0.3333333333333333), ("C", 0.5))
  }

  test("partsOf from Traversal can reverse only the first 3 elements") {
    val partsOfTraversal = Traversal.fromTraverse[List, Int].take(3).partsOf
    val result = partsOfTraversal.over(_.reverse)(List(1, 2, 3, 4, 5, 6))

    result shouldEqual List(3, 2, 1, 4, 5, 6)
  }

  test("partsOf from ATraversal can reverse only the first 3 elements") {
    val partsOfATraversal = ATraversal.fromTraverse[List, Int].take(3).partsOf
    val result = partsOfATraversal.over(_.reverse)(List(1, 2, 3, 4, 5, 6))

    result shouldEqual List(3, 2, 1, 4, 5, 6)
  }

  test("unsafePartsOf from Traversal can view focuses as List") {
    val target = List("A", "B", "C").zipWithIndex
    unsafePartsOfFromTraversal.view(target) shouldEqual List("A", "B", "C")
  }

  test("unsafePartsOf from ATraversal can view focuses as List") {
    val target = List("A", "B", "C").zipWithIndex
    unsafePartsOfFromATraversal.view(target) shouldEqual List("A", "B", "C")
  }

  test("unsafePartsOf from Traversal can set the lens to a list to replace the corresponding elements") {
    val target = List("A", "B", "C").zipWithIndex
    unsafePartsOfFromTraversal.set(List(true, false, true))(target) shouldEqual
      List((true, 0), (false, 1), (true, 2))
  }

  test("unsafePartsOf from ATraversal can set the lens to a list to replace the corresponding elements") {
    val target = List("A", "B", "C").zipWithIndex
    unsafePartsOfFromATraversal.set(List(true, false, true))(target) shouldEqual
      List((true, 0), (false, 1), (true, 2))
  }

  test("unsafePartsOf from Traversal ignores extra elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List(true, false, true, false, true, false, true)

    unsafePartsOfFromTraversal.set(replaceList)(target) shouldEqual
      List((true, 0), (false, 1), (true, 2))
  }

  test("unsafePartsOf from ATraversal ignores extra elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List(true, false, true, false, true, false, true)

    unsafePartsOfFromATraversal.set(replaceList)(target) shouldEqual
      List((true, 0), (false, 1), (true, 2))
  }

  test("unsafePartsOf from Traversal crashes, when setting the lens with a wrong number of list elements") {
    val target = List("A", "B", "C").zipWithIndex
    val thrown = intercept[IllegalArgumentException] {
      unsafePartsOfFromTraversal.set(List(true, false))(target)
    }

    thrown.getMessage shouldEqual "Not enough elements were supplied"
  }

  test("unsafePartsOf from ATraversal crashes, when setting the lens with a wrong number of list elements") {
    val target = List("A", "B", "C").zipWithIndex
    val thrown = intercept[IllegalArgumentException] {
      unsafePartsOfFromATraversal.set(List(true, false))(target)
    }

    thrown.getMessage shouldEqual "Not enough elements were supplied"
  }
}
