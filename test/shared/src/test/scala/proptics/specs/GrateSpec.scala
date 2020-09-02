package proptics.specs

import cats.instances.int._
import cats.{Applicative, Id}
import cats.instances.function._
import proptics.Grate
import proptics.law.{GrateRules, SetterTests}
import proptics.specs.Compose._

class GrateSpec extends PropticsSuite {
  val wholeGrate: Grate[Whole, Int] = Grate[Whole, Int](w2i => Whole(w2i(_.part)))
  val fromDistributive: Grate[Whole => Int, Int] = Grate.fromDistributive[Whole => *, Int]

  checkAll("Grate[Whole, Int] apply", GrateRules(wholeGrate))
  checkAll("Grate[Int, Int] id", GrateRules(Grate.id[Int]))
  checkAll("Grate[Int, Int] compose with Iso[Int, Int]", GrateRules(grate compose iso))
  checkAll("Grate[Int, Int] compose with AnIso[Int, Int]", GrateRules(grate compose anIso))
  checkAll("Grate[Int, Int] compose with Setter[Int, Int]", SetterTests(grate compose setter).setter)
  checkAll("Grate[Int, Int] compose with Grate[Int, Int]", GrateRules(grate compose grate))

  test("review") {
    wholeGrate.review(9) shouldEqual whole9
    fromDistributive.review(9)(Whole(1)) shouldEqual 9
  }

  test("set") {
    wholeGrate.set(9)(Whole(1)) shouldEqual whole9
    fromDistributive.set(9)(_.part + 1)(Whole(1)) shouldEqual 9
  }

  test("over") {
    wholeGrate.over(_ + 1)(Whole(8)) shouldEqual whole9
    fromDistributive.over(_ + 1)(_.part)(Whole(8)) shouldEqual 9
  }

  test("zipWith") {
    wholeGrate.zipWith(Whole(8), Whole(1))(_ + _) shouldEqual whole9
    fromDistributive.zipWith(_.part, _.part)(_ + _ + 1)(Whole(4)) shouldEqual 9
  }
  test("cotraverse") {
    val cotraversedWhole = wholeGrate.cotraverse[Id](whole9)(identity)
    val fromDistributiveCotraverse = fromDistributive.cotraverse[Id](_.part)(identity)

    cotraversedWhole shouldEqual whole9
    fromDistributiveCotraverse(whole9) shouldEqual 9
    wholeGrate.zipWithF[Id](identity)(whole9) shouldEqual cotraversedWhole
    fromDistributive.zipWithF[Id](identity)(_.part)(Applicative[Id])(whole9) shouldEqual
      fromDistributiveCotraverse(whole9)
  }

  test("compose with Review") {
    (grate compose review).review(9) shouldEqual 9
  }
}
