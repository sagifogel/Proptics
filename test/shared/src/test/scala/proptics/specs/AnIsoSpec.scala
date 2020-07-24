package proptics.specs

import cats.Id
import cats.instances.function._
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._
import org.scalacheck.Arbitrary._
import org.typelevel.discipline.Laws
import proptics.AnIso
import proptics.internal.Exchange
import proptics.law.{AnIsoRules, IsoRules}

class AnIsoSpec extends PropticsSuite {
  val anIso: AnIso[Whole, Int] = AnIso[Whole, Int](_.part)(Whole.apply)
  val ruleSetIdentityAnIso: Laws#RuleSet = AnIsoRules(AnIso[Int, Int](identity[Int])(identity))
  def ruleSetApply(anIso: AnIso[Whole, Int]): Laws#RuleSet = AnIsoRules(anIso)

  checkAll("AnIso apply ", ruleSetApply(anIso))
  checkAll("AnIso identity", ruleSetIdentityAnIso)
  checkAll("AnIso reverse twice", ruleSetApply(anIso.reverse.reverse))
  checkAll("AnIso asIso", IsoRules(anIso.asIso))

  test("view") {
    anIso.view(whole9) shouldEqual 9
  }

  test("review") {
    anIso.review(9) shouldEqual whole9
  }

  test("set") {
    anIso.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    anIso.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
  test("traverse") {
    anIso.traverse(whole9)(_.some) shouldEqual Some(whole9)
    anIso.traverse(whole9)(_.some) shouldEqual anIso.overF(_.some)(whole9)
  }

  test("exists") {
    anIso.exists(greaterThan5)(whole9) shouldEqual true
    anIso.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    anIso.notExists(greaterThan10)(whole9) shouldEqual true
    anIso.notExists(greaterThan5)(whole9) shouldEqual false
    anIso.notExists(greaterThan5)(whole9) shouldEqual (!anIso.exists(greaterThan5)(whole9))
  }

  test("contains") {
    anIso.contains(whole9)(9) shouldEqual true
    anIso.contains(whole9)(5) shouldEqual false
  }

  test("notContains") {
    anIso.notContains(whole9)(5) shouldEqual true
    anIso.notContains(whole9)(9) shouldEqual false
    anIso.notContains(whole9)(9) shouldEqual (!anIso.contains(whole9)(9))
  }

  test("find") {
    anIso.find(greaterThan5)(whole9) shouldEqual Some(9)
    anIso.find(greaterThan10)(whole9) shouldEqual None
  }

  test("withIso") {
    val exchange = anIso.withIso[Exchange[Int, Int, Whole, Whole]](s2a => b2t => Exchange(s2a, b2t))

    exchange.view(whole9) shouldEqual 9
    exchange.review(9) shouldEqual whole9
  }

  test("use") {
    anIso.use.runA(whole9).value shouldEqual 9
  }

  test("cotraverse") {
    val cotraversedWhole = anIso.cotraverse[Id](whole9)(identity)

    cotraversedWhole shouldEqual whole9
    anIso.zipWithF[Id](identity)(whole9) shouldEqual cotraversedWhole
  }

  test("au") {
    anIso.au[String](focus2Whole => s => focus2Whole(s.toInt))("9") shouldEqual 9
  }

  test("auf") {
    val fn = anIso.auf[* => *, String, Int]((f, s) => f(s.toInt))(Whole.apply)
    fn("9") shouldEqual whole9
  }

  test("under") {
    anIso.under(w => w.copy(part = w.part + 1))(8) shouldEqual 9
  }

  test("mapping") {
    val iso = anIso.mapping[Id, Option]

    iso.view(whole9) shouldEqual 9
    iso.review(9.some) shouldEqual whole9.some
  }

  test("dimapping") {
    val reversed = anIso.reverse
    val liftedAnIso = anIso.dimapping[* => *, * => *, Int, Int, Whole, Whole](reversed)

    liftedAnIso.view(_ + 1)(Whole(8)) shouldEqual whole9
    liftedAnIso.review(whole => whole.copy(whole.part + 1))(8) shouldEqual 9
  }
}
