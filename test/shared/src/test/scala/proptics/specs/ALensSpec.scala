package proptics.specs

import cats.instances.int._
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.bifunctor._
import cats.syntax.option._
import org.scalacheck.Arbitrary._
import proptics.instances.tuple._
import proptics.internal.Shop
import proptics.law._
import proptics.specs.compose._
import proptics.{ALens, Lens}

class ALensSpec extends PropticsSuite {
  val wholeLens: ALens[Whole, Int] = ALens[Whole, Int](_.part)(w => i => w.copy(part = i))

  checkAll("ALens[Whole, Int] apply", ALensTests(wholeLens).aLens)
  checkAll("ALens[Whole, Int] asLens", LensTests(wholeLens.asLens).lens)
  checkAll("ALens[Int, Int] id", ALensTests(ALens.id[Int]).aLens)
  checkAll("ALens[Int, Int] compose with Iso[Int, Int]", ALensTests(aLens compose iso).aLens)
  checkAll("ALens[Int, Int] compose with AnIso[Int, Int]", ALensTests(aLens compose anIso).aLens)
  checkAll("ALens[Int, Int] compose with Lens[Int, Int]", ALensTests(aLens compose lens).aLens)
  checkAll("ALens[Int, Int] compose with ALens[Int, Int]", ALensTests(aLens compose aLens).aLens)
  checkAll("ALens[Int, Int] compose with Prism[Int, Int]", TraversalTests(aLens compose prism).traversal)
  checkAll("ALens[Int, Int] compose with APrism[Int, Int]", TraversalTests(aLens compose aPrism).traversal)
  checkAll("ALens[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(aLens compose affineTraversal).affineTraversal)
  checkAll("ALens[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(aLens compose anAffineTraversal).anAffineTraversal)
  checkAll("ALens[Int, Int] compose with Traversal[Int, Int]", TraversalTests(aLens compose traversal).traversal)
  checkAll("ALens[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(aLens compose aTraversal).aTraversal)
  checkAll("ALens[Int, Int] compose with Setter[Int, Int]", SetterTests(aLens compose setter).setter)

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
    wholeLens.contains(whole9)(9) shouldEqual true
    wholeLens.contains(whole9)(5) shouldEqual false
  }

  test("notContains") {
    wholeLens.notContains(whole9)(5) shouldEqual true
    wholeLens.notContains(whole9)(9) shouldEqual false
    wholeLens.notContains(whole9)(9) shouldEqual (!wholeLens.contains(whole9)(9))
  }

  test("use") {
    wholeLens.use.runA(whole9).value shouldEqual 9
  }

  test("withLens") {
    val shop = wholeLens.withLens[Shop[Int, Int, Whole, Whole]](get => set => Shop(get, set))

    shop.set(whole9)(0) shouldEqual Whole(0)
  }

  test("lensStore") {
    sealed trait ADT
    case class IntWrapper(value: Int) extends ADT
    case class TupleWrapper(value: (Boolean, Int)) extends ADT

    val adtLens: Lens[ADT, Int] = Lens.lens {
      case IntWrapper(value) =>
        ALens.id[Int].lensStore(value).bimap(identity, fn => i => IntWrapper(fn(i)))
      case TupleWrapper(value) =>
        _2A[Int, Int, Boolean].lensStore(value).bimap(identity, fn => i => TupleWrapper(fn(i)))
    }

    adtLens.view(IntWrapper(9)) shouldEqual 9
    adtLens.view(TupleWrapper((true, 9))) shouldEqual 9
    adtLens.set(9)(IntWrapper(0)) shouldEqual IntWrapper(9)
    adtLens.set(9)(TupleWrapper((true, 0))) shouldEqual TupleWrapper((true, 9))
  }

  test("compose with Getter") {
    (aLens compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (aLens compose fold).fold(9) shouldEqual 9
  }
}
