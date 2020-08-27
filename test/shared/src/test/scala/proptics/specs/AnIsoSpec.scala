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
import proptics.law._
import proptics.specs.Compose._

class AnIsoSpec extends PropticsSuite {
  val wholeIso: AnIso[Whole, Int] = AnIso[Whole, Int](_.part)(Whole.apply)
  val ruleSetIdentityAnIso: Laws#RuleSet = AnIsoRules(AnIso[Int, Int](identity[Int])(identity))
  def ruleSetApply(anIso: AnIso[Whole, Int]): Laws#RuleSet = AnIsoRules(anIso)

  checkAll("AnIso apply ", ruleSetApply(wholeIso))
  checkAll("AnIso identity", ruleSetIdentityAnIso)
  checkAll("AnIso reverse twice", ruleSetApply(wholeIso.reverse.reverse))
  checkAll("AnIso asIso", IsoRules(wholeIso.asIso))
  checkAll("compose with Iso", AnIsoRules(anIso compose iso))
  checkAll("compose with AnIso", AnIsoRules(anIso compose anIso))
  checkAll("compose with Lens", LensRules(anIso compose lens))
  checkAll("compose with ALens", ALensRules(anIso compose aLens))
  checkAll("compose with Prism", PrismRules(anIso compose prism))
  checkAll("compose with APrism", APrismRules(anIso compose aPrism))
  checkAll("compose with AffineTraversal", AffineTraversalRules(anIso compose affineTraversal))
  checkAll("compose with AnAffineTraversal", AnAffineTraversalRules(anIso compose anAffineTraversal))
  checkAll("compose with Traversal", TraversalRules(anIso compose traversal))
  checkAll("compose with ATraversal", ATraversalRules(anIso compose aTraversal))
  checkAll("compose with Setter", SetterRules(anIso compose setter))
  checkAll("compose with Grate", GrateRules(anIso compose grate))

  test("view") {
    wholeIso.view(whole9) shouldEqual 9
  }

  test("review") {
    wholeIso.review(9) shouldEqual whole9
  }

  test("set") {
    wholeIso.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    wholeIso.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
  test("traverse") {
    wholeIso.traverse(whole9)(_.some) shouldEqual Some(whole9)
    wholeIso.traverse(whole9)(_.some) shouldEqual wholeIso.overF(_.some)(whole9)
  }

  test("exists") {
    wholeIso.exists(greaterThan5)(whole9) shouldEqual true
    wholeIso.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    wholeIso.notExists(greaterThan10)(whole9) shouldEqual true
    wholeIso.notExists(greaterThan5)(whole9) shouldEqual false
    wholeIso.notExists(greaterThan5)(whole9) shouldEqual (!wholeIso.exists(greaterThan5)(whole9))
  }

  test("contains") {
    wholeIso.contains(whole9)(9) shouldEqual true
    wholeIso.contains(whole9)(5) shouldEqual false
  }

  test("notContains") {
    wholeIso.notContains(whole9)(5) shouldEqual true
    wholeIso.notContains(whole9)(9) shouldEqual false
    wholeIso.notContains(whole9)(9) shouldEqual (!wholeIso.contains(whole9)(9))
  }

  test("find") {
    wholeIso.find(greaterThan5)(whole9) shouldEqual Some(9)
    wholeIso.find(greaterThan10)(whole9) shouldEqual None
  }

  test("withIso") {
    val exchange = wholeIso.withIso[Exchange[Int, Int, Whole, Whole]](s2a => b2t => Exchange(s2a, b2t))

    exchange.view(whole9) shouldEqual 9
    exchange.review(9) shouldEqual whole9
  }

  test("use") {
    wholeIso.use.runA(whole9).value shouldEqual 9
  }

  test("cotraverse") {
    val cotraversedWhole = wholeIso.cotraverse[Id](whole9)(identity)

    cotraversedWhole shouldEqual whole9
    wholeIso.zipWithF[Id](identity)(whole9) shouldEqual cotraversedWhole
  }

  test("au") {
    wholeIso.au[String](focus2Whole => s => focus2Whole(s.toInt))("9") shouldEqual 9
  }

  test("auf") {
    val fn = wholeIso.auf[* => *, String, Int]((f, s) => f(s.toInt))(Whole.apply)
    fn("9") shouldEqual whole9
  }

  test("under") {
    wholeIso.under(w => w.copy(part = w.part + 1))(8) shouldEqual 9
  }

  test("mapping") {
    val iso = wholeIso.mapping[Id, Option]

    iso.view(whole9) shouldEqual 9
    iso.review(9.some) shouldEqual whole9.some
  }

  test("dimapping") {
    val reversed = wholeIso.reverse
    val liftedAnIso = wholeIso.dimapping[* => *, * => *, Int, Int, Whole, Whole](reversed)

    liftedAnIso.view(_ + 1)(Whole(8)) shouldEqual whole9
    liftedAnIso.review(whole => whole.copy(whole.part + 1))(8) shouldEqual 9
  }

  test("compose with Getter") {
    (anIso compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (anIso compose fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (anIso compose review).review(9) shouldEqual 9
  }
}
