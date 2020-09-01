package proptics.specs

import cats.Id
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._
import org.scalacheck.Arbitrary._
import proptics.Lens
import proptics.law._
import proptics.specs.Compose._

import scala.Function.const

class LensSpec extends PropticsSuite {
  val wholeLens: Lens[Whole, Int] = Lens[Whole, Int](_.part)(w => i => w.copy(part = i))
  val identityLens: Lens[Int, Int] = Lens[Int, Int](identity)(const(identity))

  checkAll("Lens[Whole, Int] apply", LensTests(wholeLens).lens)
  checkAll("Lens[Int, Int] identity", LensTests(identityLens).lens)
  checkAll("Lens[Int, Int] compose with Iso[Int, Int]", LensTests(lens compose iso).lens)
  checkAll("Lens[Int, Int] compose with AnIso[Int, Int]", LensTests(lens compose anIso).lens)
  checkAll("Lens[Int, Int] compose with Lens[Int, Int]", LensTests(lens compose lens).lens)
  checkAll("Lens[Int, Int] compose with ALens[Int, Int]", ALensTests(lens compose aLens).aLens)
  checkAll("compose with Prism", TraversalRules(lens compose prism))
  checkAll("compose with APrism", TraversalRules(lens compose aPrism))
  checkAll("compose with AffineTraversal", AffineTraversalRules(lens compose affineTraversal))
  checkAll("compose with AnAffineTraversal", AnAffineTraversalRules(lens compose anAffineTraversal))
  checkAll("compose with Traversal", TraversalRules(lens compose traversal))
  checkAll("compose with ATraversal", ATraversalRules(lens compose aTraversal))
  checkAll("compose with Setter", SetterRules(lens compose setter))

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
}
