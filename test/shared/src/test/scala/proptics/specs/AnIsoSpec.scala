package proptics.specs

import cats.Id
import cats.instances.function._
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._
import org.scalacheck.Arbitrary._
import proptics.AnIso
import proptics.internal.Exchange
import proptics.law._
import proptics.specs.Compose._

class AnIsoSpec extends PropticsSuite {
  val wholeIso: AnIso[Whole, Int] = AnIso[Whole, Int](_.part)(Whole.apply)

  checkAll("AnIso[Whole, Int] apply ", AnIsoTests(wholeIso).anIso)
  checkAll("AnIso[Whole, Int] reverse twice", AnIsoTests(wholeIso.reverse.reverse).anIso)
  checkAll("AnIso[Whole, Int] asIso", IsoTests(wholeIso.asIso).iso)
  checkAll("AnIso[Int, Int] id", AnIsoTests(AnIso.id[Int]).anIso)
  checkAll("AnIso[Int, Int] compose with Iso[Int, Int]", AnIsoTests(anIso compose iso).anIso)
  checkAll("AnIso[Int, Int] compose with AnIso[Int, Int]", AnIsoTests(anIso compose anIso).anIso)
  checkAll("AnIso[Int, Int] compose with Lens[Int, Int]", LensTests(anIso compose lens).lens)
  checkAll("AnIso[Int, Int] compose with ALens[Int, Int] ", ALensTests(anIso compose aLens).aLens)
  checkAll("AnIso[Int, Int] compose with Prism[Int, Int]", PrismTests(anIso compose prism).prism)
  checkAll("AnIso[Int, Int] compose with APrism[Int, Int]", APrismTests(anIso compose aPrism).aPrism)
  checkAll("AnIso[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalRules(anIso compose affineTraversal))
  checkAll("AnIso[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalRules(anIso compose anAffineTraversal))
  checkAll("AnIso[Int, Int] compose with Traversal[Int, Int]", TraversalRules(anIso compose traversal))
  checkAll("AnIso[Int, Int] compose with ATraversal[Int, Int]", ATraversalRules(anIso compose aTraversal))
  checkAll("AnIso[Int, Int] compose with Setter[Int, Int]", SetterRules(anIso compose setter))
  checkAll("AnIso[Int, Int] compose with Grate[Int, Int]", GrateRules(anIso compose grate))

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
