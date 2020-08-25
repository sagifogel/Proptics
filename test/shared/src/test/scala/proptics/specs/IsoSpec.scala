package proptics.specs

import cats.Id
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._
import org.scalacheck.Arbitrary._
import org.typelevel.discipline.Laws
import proptics.law.{ALensRules, ATraversalRules, AffineTraversalRules, AnAffineTraversalRules, GrateRules, IsoRules, LensRules, PrismRules, SetterRules, TraversalRules}
import proptics.specs.Compose._
import proptics.{Iso, Iso_}

class IsoSpec extends PropticsSuite {
  val iso: Iso[Whole, Int] = Iso.iso[Whole, Int](_.part)(Whole.apply)
  def ruleSetApply(iso: Iso[Whole, Int]): Laws#RuleSet = IsoRules(iso)
  val identityIso: Iso[Int, Int] = Iso[Int, Int](identity[Int] _)(identity)
  val ruleSetIdentityIso: Laws#RuleSet = IsoRules(identityIso)
  val combineFocus: (Whole, Whole) => Int = { case (whole1, whole2) => whole1.part + whole2.part }
  val flipped: Iso_[Whole => Int => Int, Whole => Int => Int, Int => Whole => Int, Int => Whole => Int] = Iso_.flipped
  val curried: Iso_[(Whole, Whole) => Int, (Whole, Whole) => Int, Whole => Whole => Int, Whole => Whole => Int] = Iso_.curried
  val uncurried: Iso_[Whole => Whole => Int, Whole => Whole => Int, (Whole, Whole) => Int, (Whole, Whole) => Int] = Iso_.uncurried

  checkAll("Iso apply", ruleSetApply(iso))
  checkAll("Iso identity", ruleSetIdentityIso)
  checkAll("Iso id", IsoRules(Iso.id[Int]))
  checkAll("Iso reverse twice", ruleSetApply(iso.reverse.reverse))

  test("view") {
    iso.view(whole9) shouldEqual 9
  }

  test("review") {
    iso.review(9) shouldEqual whole9
  }

  test("set") {
    iso.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    iso.over(_ + 1)(Whole(8)) shouldEqual whole9
  }

  test("traverse") {
    iso.traverse(whole9)(_.some) shouldEqual whole9.some
    iso.traverse(whole9)(_.some) shouldEqual iso.overF(_.some)(whole9)
  }

  test("exists") {
    iso.exists(greaterThan5)(whole9) shouldEqual true
    iso.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    iso.notExists(greaterThan10)(whole9) shouldEqual true
    iso.notExists(greaterThan5)(whole9) shouldEqual false
    iso.notExists(greaterThan5)(whole9) shouldEqual (!iso.exists(greaterThan5)(whole9))
  }

  test("contains") {
    iso.contains(whole9)(9) shouldEqual true
    iso.contains(whole9)(5) shouldEqual false
  }

  test("notContains") {
    iso.notContains(whole9)(5) shouldEqual true
    iso.notContains(whole9)(9) shouldEqual false
    iso.notContains(whole9)(9) shouldEqual (!iso.contains(whole9)(9))
  }

  test("find") {
    iso.find(greaterThan5)(whole9) shouldEqual Some(9)
    iso.find(greaterThan10)(whole9) shouldEqual None
  }

  test("use") {
    iso.use.runA(whole9).value shouldEqual 9
  }

  test("zipWith") {
    iso.zipWith(Whole(8), Whole(1))(_ + _) shouldEqual whole9
  }

  test("cotraverse") {
    val cotraversedWhole = iso.cotraverse[Id](whole9)(identity)

    cotraversedWhole shouldEqual whole9
    iso.zipWithF[Id](identity)(whole9) shouldEqual cotraversedWhole
  }

  test("curried") {
    curried.view(combineFocus)(whole9)(whole9) shouldEqual 18
    curried.review(combineFocus curried)(whole9, whole9) shouldEqual 18
  }

  test("uncurried") {
    uncurried.view(combineFocus curried)(whole9, whole9) shouldEqual 18
    uncurried.review(combineFocus)(whole9)(whole9) shouldEqual 18
  }

  test("flipped") {
    flipped.view(w => w.part + _)(9)(whole9) shouldEqual 18
  }

  checkAll("compose with Iso", IsoRules(identityIso compose identityIso))
  checkAll("compose with Lens", LensRules(identityIso compose lens))
  checkAll("compose with ALens", ALensRules(identityIso compose aLens))
  checkAll("compose with Prism", PrismRules(identityIso compose prism))
  checkAll("compose with AffineTraversal", AffineTraversalRules(identityIso compose affineTraversal))
  checkAll("compose with AnAffineTraversal", AnAffineTraversalRules(identityIso compose anAffineTraversal))
  checkAll("compose with Traversal", TraversalRules(identityIso compose traversal))
  checkAll("compose with ATraversal", ATraversalRules(identityIso compose aTraversal))
  checkAll("compose with Setter", SetterRules(identityIso compose setter))
  checkAll("compose with Grate", GrateRules(identityIso compose grate))

  test("compose with Getter") {
    (identityIso compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (identityIso compose fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (identityIso compose review).review(9) shouldEqual 9
  }
}
