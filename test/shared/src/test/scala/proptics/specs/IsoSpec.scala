package proptics.specs

import cats.Id
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._
import org.scalacheck.Arbitrary._
import org.typelevel.discipline.Laws
import proptics.{Iso, Iso_}
import proptics.law.IsoRules

class IsoSpec extends PropticsSuite {
  val iso: Iso[Whole, Int] = Iso.iso[Whole, Int](_.focus)(Whole.apply)
  def ruleSetApply(iso: Iso[Whole, Int]): Laws#RuleSet = IsoRules(iso)
  val ruleSetIdentityIso: Laws#RuleSet = IsoRules(Iso[Int, Int](identity[Int] _)(identity))
  val combineFocus: (Whole, Whole) => Int = { case (whole1, whole2) => whole1.focus + whole2.focus }
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
    flipped.view(w => w.focus + _)(9)(whole9) shouldEqual 18
  }
}
