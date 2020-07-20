package proptics.specs

import cats.Id
import cats.syntax.option._
import cats.instances.option._
import cats.instances.int._
import org.scalacheck.Arbitrary._
import org.typelevel.discipline.Laws
import proptics.Lens
import proptics.law.LensRules

import scala.Function.const

class LensSpec extends PropticsSuite {
  val lens: Lens[Whole, Int] = Lens[Whole, Int](_.part)(w => i => w.copy(part = i))
  val ruleSetIdentityLens: Laws#RuleSet = LensRules(Lens[Int, Int](identity)(const(identity)))
  def ruleSetApply(lens: Lens[Whole, Int]): Laws#RuleSet = LensRules(lens)

  checkAll("Lens apply", ruleSetApply(lens))
  checkAll("Lens identity", ruleSetIdentityLens)

  test("view") {
    lens.view(whole9) shouldEqual 9
  }

  test("set") {
    lens.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    lens.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
  test("traverse") {
    lens.traverse(whole9)(_.some) shouldEqual Some(whole9)
    lens.traverse(whole9)(_.some) shouldEqual lens.overF(_.some)(whole9)
  }

  test("find") {
    lens.find(greaterThan5)(whole9) shouldEqual Some(9)
    lens.find(greaterThan10)(whole9) shouldEqual None
  }

  test("exists") {
    lens.exists(greaterThan5)(whole9) shouldEqual true
    lens.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    lens.notExists(greaterThan10)(whole9) shouldEqual true
    lens.notExists(greaterThan5)(whole9) shouldEqual false
    lens.notExists(greaterThan5)(whole9) shouldEqual (!lens.exists(greaterThan5)(whole9))
  }

  test("contains") {
    lens.contains(whole9)(9) shouldEqual true
    lens.contains(whole9)(5) shouldEqual false
  }

  test("notContains") {
    lens.notContains(whole9)(5) shouldEqual true
    lens.notContains(whole9)(9) shouldEqual false
    lens.notContains(whole9)(9) shouldEqual (!lens.contains(whole9)(9))
  }

  test("use") {
    lens.use.runA(whole9).value shouldEqual 9
  }

  test("failover") {
    val res = lens.failover[Option](identity)(whole9)(strongStarTupleOfDisj, catsStdInstancesForOption)
    val negativeRes = lens.failover[Option](identity)(whole9)(strongStarTupleOfNegativeDisj, catsStdInstancesForOption)

    res shouldEqual Some(whole9)
    negativeRes shouldEqual None
  }

  test("zipWith") {
    lens.zipWith(Whole(8), Whole(1))(_ + _) shouldEqual whole9
  }

  test("cotraverse") {
    val cotraversedWhole = lens.cotraverse[Id](whole9)(identity)

    cotraversedWhole shouldEqual whole9
    lens.zipWithF[Id](identity)(whole9) shouldEqual cotraversedWhole
  }
}
