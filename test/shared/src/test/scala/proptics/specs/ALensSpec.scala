package proptics.specs
import cats.Eq
import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.syntax.bifunctor._
import cats.syntax.option._
import org.scalacheck.Arbitrary._

import proptics.instances.field1._
import proptics.instances.field2._
import proptics.internal.Shop
import proptics.law.discipline._
import proptics.specs.compose._
import proptics.std.tuple._
import proptics.{ALens, Lens, Prism}

class ALensSpec extends PropticsSuite {
  val wholeLens: ALens[Whole, Int] = ALens[Whole, Int](_.part)(w => i => w.copy(part = i))
  implicit def eqArrow(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => Int] = Eq.instance[Int => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int) === f2(int)
    }
  }

  checkAll("ALens[Int, Int] id", ALensTests(ALens.id[Int]).aLens)
  checkAll("ALens[Whole, Int] apply", ALensTests(wholeLens).aLens)
  checkAll("ALens[Whole, Int] asLens", LensTests(wholeLens.asLens).lens)
  checkAll("ALens[Int, Int] asALens", ALensTests(Lens.id[Int].asALens).aLens)
  checkAll("ALens[(Int, String), Int] _1A", ALensTests(_1A[Int, String]).aLens)
  checkAll("ALens[(Int, String), String] _2A", ALensTests(_2A[Int, String]).aLens)
  checkAll("ALens[Int, Int] compose with Iso[Int, Int]", ALensTests(aLens compose iso).aLens)
  checkAll("ALens[Int, Int] andThen with Iso[Int, Int]", ALensTests(aLens andThen iso).aLens)
  checkAll("ALens[Int, Int] compose with Lens[Int, Int]", ALensTests(aLens compose lens).aLens)
  checkAll("ALens[Int, Int] andThen with Lens[Int, Int]", ALensTests(aLens andThen lens).aLens)
  checkAll("ALens[(Int, Int), (Int, Int), Int, Int] _1P", ALensTests(_1PA[Int, Int, Int]).aLens)
  checkAll("ALens[(Int, Int), (Int, Int), Int, Int] _2P", ALensTests(_2PA[Int, Int, Int]).aLens)
  checkAll("ALens[Int, Int] compose with AnIso[Int, Int]", ALensTests(aLens compose anIso).aLens)
  checkAll("ALens[Int, Int] andThen with AnIso[Int, Int]", ALensTests(aLens andThen anIso).aLens)
  checkAll("ALens[Int, Int] compose with ALens[Int, Int]", ALensTests(aLens compose aLens).aLens)
  checkAll("ALens[Int, Int] andThen with ALens[Int, Int]", ALensTests(aLens andThen aLens).aLens)
  checkAll("ALens[Int => Int, Int => Int] outside", ALensTests(ALens.outside[Int, Int, Int](Prism.id[Int])).aLens)
  checkAll("ALens[Int, Int] compose with Prism[Int, Int]", AffineTraversalTests(aLens compose prism).affineTraversal)
  checkAll("ALens[Int, Int] andThen with Prism[Int, Int]", AffineTraversalTests(aLens andThen prism).affineTraversal)
  checkAll("ALens[Int, Int] compose with APrism[Int, Int]", AffineTraversalTests(aLens compose aPrism).affineTraversal)
  checkAll("ALens[Int, Int] andThen with APrism[Int, Int]", AffineTraversalTests(aLens andThen aPrism).affineTraversal)
  checkAll("ALens[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(aLens compose affineTraversal).affineTraversal)
  checkAll("ALens[Int, Int] andThen with AffineTraversal[Int, Int]", AffineTraversalTests(aLens andThen affineTraversal).affineTraversal)
  checkAll("ALens[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(aLens compose anAffineTraversal).anAffineTraversal)
  checkAll("ALens[Int, Int] andThen with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(aLens andThen anAffineTraversal).anAffineTraversal)
  checkAll("ALens[Int, Int] compose with Traversal[Int, Int]", TraversalTests(aLens compose traversal).traversal)
  checkAll("ALens[Int, Int] andThen with Traversal[Int, Int]", TraversalTests(aLens andThen traversal).traversal)
  checkAll("ALens[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(aLens compose aTraversal).aTraversal)
  checkAll("ALens[Int, Int] andThen with ATraversal[Int, Int]", ATraversalTests(aLens andThen aTraversal).aTraversal)
  checkAll("ALens[Int, Int] compose with Setter[Int, Int]", SetterTests(aLens compose setter).setter)
  checkAll("ALens[Int, Int] andThen with Setter[Int, Int]", SetterTests(aLens andThen setter).setter)
  checkAll("ALens[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedLensTests(aLens compose indexedLens).indexedLens)
  checkAll("ALens[Int, Int] andThen with IndexedLens[Int, Int, Int]", IndexedLensTests(aLens andThen indexedLens).indexedLens)
  checkAll("ALens[Int, Int] compose with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(aLens compose anIndexedLens).anIndexedLens)
  checkAll("ALens[Int, Int] andThen with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(aLens andThen anIndexedLens).anIndexedLens)
  checkAll("ALens[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(aLens compose indexedTraversal).indexedTraversal)
  checkAll("ALens[Int, Int] andThen with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(aLens andThen indexedTraversal).indexedTraversal)
  checkAll("ALens[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(aLens compose indexedSetter).indexedSetter)
  checkAll("ALens[Int, Int] andThen with IndexedSetter[Int, Int, Int]", IndexedSetterTests(aLens andThen indexedSetter).indexedSetter)

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

  test("withLens") {
    val shop: Shop[Int, Int, Whole, Whole] = wholeLens.toShop

    shop.set(whole9)(0) shouldEqual Whole(0)
  }

  test("lensStore") {
    sealed trait ADT
    case class IntWrapper(value: Int) extends ADT
    case class TupleWrapper(value: (Boolean, Int)) extends ADT

    val adtLens: Lens[ADT, Int] = Lens.lens {
      case IntWrapper(value) =>
        ALens.id[Int].lensStore(value).bimap(identity, fn => IntWrapper.apply _ compose fn)
      case TupleWrapper(value) =>
        _2A[Boolean, Int].lensStore(value).bimap(identity, fn => TupleWrapper.apply _ compose fn)
    }

    adtLens.view(IntWrapper(9)) shouldEqual 9
    adtLens.view(TupleWrapper((true, 9))) shouldEqual 9
    adtLens.set(9)(IntWrapper(0)) shouldEqual IntWrapper(9)
    adtLens.set(9)(TupleWrapper((true, 0))) shouldEqual TupleWrapper((true, 9))
  }

  test("compose with Getter") {
    (aLens compose getter).view(9) shouldEqual 9
  }

  test("andThen with Getter") {
    (aLens andThen getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (aLens compose fold).fold(9) shouldEqual 9
  }

  test("andThen with Fold") {
    (aLens andThen fold).fold(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = aLens compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter") {
    val composed = aLens andThen indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = aLens compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedFold") {
    val composed = aLens andThen indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
