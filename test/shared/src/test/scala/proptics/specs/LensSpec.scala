package proptics.specs

import cats.instances.option.catsStdInstancesForOption
import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.syntax.option._
import cats.{Eq, Id}
import org.scalacheck.Arbitrary._

import proptics.instances.fields._
import proptics.law.discipline._
import proptics.specs.compose._
import proptics.std.tuple._
import proptics.{Lens, Prism}

class LensSpec extends PropticsSuite {
  val wholeLens: Lens[Whole, Int] = Lens[Whole, Int](_.part)(w => i => w.copy(part = i))
  implicit def eqArrow(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => Int] = Eq.instance[Int => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int) === f2(int)
    }
  }

  checkAll("Lens[Int, Int] id", LensTests(Lens.id[Int]).lens)
  checkAll("Lens[Whole, Int] apply", LensTests(wholeLens).lens)
  checkAll("Lens[(Int, Int), Int] first", Field1Tests[Int, Int].first)
  checkAll("Lens[(Int, Int), Int] first", Field2Tests[Int, Int].second)
  checkAll("Lens[(Int, String), Int] _1", LensTests(_1[Int, String]).lens)
  checkAll("Lens[(Int, Int), Int] first", Field3Tests[Int, Int, Int].third)
  checkAll("Lens[(Int, String), String] _2", LensTests(_2[Int, String]).lens)
  checkAll("Lens[(Int, Int), Int] first", Field4Tests[Int, Int, Int, Int].fourth)
  checkAll("Lens[(Int, Int), Int] first", Field5Tests[Int, Int, Int, Int, Int].fifth)
  checkAll("Lens[Int, Int] compose with Iso[Int, Int]", LensTests(lens compose iso).lens)
  checkAll("Lens[Int, Int] compose with Lens[Int, Int]", LensTests(lens compose lens).lens)
  checkAll("Lens[(Int, Int), (Int, Int), Int, Int] _1P", LensTests(_1P[Int, Int, Int]).lens)
  checkAll("Lens[(Int, Int), (Int, Int), Int, Int] _2P", LensTests(_2P[Int, Int, Int]).lens)
  checkAll("Lens[Int, Int] compose with AnIso[Int, Int]", LensTests(lens compose anIso).lens)
  checkAll("Lens[Int, Int] compose with ALens[Int, Int]", ALensTests(lens compose aLens).aLens)
  checkAll("Lens[Int => Int, Int => Int] outside", LensTests(Lens.outside[Int, Int, Int](Prism.id[Int])).lens)
  checkAll("Lens[Int, Int] compose with Prism[Int, Int]", AffineTraversalTests(lens compose prism).affineTraversal)
  checkAll("Lens[Int, Int] compose with APrism[Int, Int]", AffineTraversalTests(lens compose aPrism).affineTraversal)
  checkAll("Lens[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(lens compose affineTraversal).affineTraversal)
  checkAll("Lens[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(lens compose anAffineTraversal).anAffineTraversal)
  checkAll("Lens[Int, Int] compose with Traversal[Int, Int]", TraversalTests(lens compose traversal).traversal)
  checkAll("Lens[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(lens compose aTraversal).aTraversal)
  checkAll("Lens[Int, Int] compose with Setter[Int, Int]", SetterTests(lens compose setter).setter)
  checkAll("Lens[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedLensTests(lens compose indexedLens).indexedLens)
  checkAll("Lens[Int, Int] compose with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(lens compose anIndexedLens).anIndexedLens)
  checkAll("Lens[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(lens compose indexedTraversal).indexedTraversal)
  checkAll("Lens[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(lens compose indexedSetter).indexedSetter)

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

  test("compose with Fold") {
    (lens compose fold).fold(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = lens compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = lens compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
