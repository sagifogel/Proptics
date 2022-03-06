package proptics.specs

import cats.{Applicative, Id}

import proptics.Grate
import proptics.law.discipline._
import proptics.specs.compose._

class GrateSpec extends PropticsSuite {
  val wholeGrate: Grate[Whole, Int] = Grate[Whole, Int](w2i => Whole(w2i(_.part)))
  val fromDistributive: Grate[Whole => Int, Int] = Grate.fromDistributive[Whole => *, Int]

  checkAll("Grate[Whole, Int] apply", GrateTests(wholeGrate).grate)
  checkAll("Grate[Int, Int] id", GrateTests(Grate.id[Int]).grate)
  checkAll("Grate[Int, Int] compose with Iso[Int, Int]", GrateTests(grate compose iso).grate)
  checkAll("Grate[Int, Int] andThen with Iso[Int, Int]", GrateTests(grate andThen iso).grate)
  checkAll("Grate[Int, Int] compose with AnIso[Int, Int]", GrateTests(grate compose anIso).grate)
  checkAll("Grate[Int, Int] andThen with AnIso[Int, Int]", GrateTests(grate andThen anIso).grate)
  checkAll("Grate[Int, Int] compose with Setter[Int, Int]", SetterTests(grate compose setter).setter)
  checkAll("Grate[Int, Int] andThen with Setter[Int, Int]", SetterTests(grate andThen setter).setter)
  checkAll("Grate[Int, Int] compose with Grate[Int, Int]", GrateTests(grate compose grate).grate)
  checkAll("Grate[Int, Int] andThen with Grate[Int, Int]", GrateTests(grate andThen grate).grate)
  checkAll("Grate[Int, Int] compose with IndexedSetter[Int, Int]", IndexedSetterTests(grate compose indexedSetter).indexedSetter)
  checkAll("Grate[Int, Int] andThen with IndexedSetter[Int, Int]", IndexedSetterTests(grate andThen indexedSetter).indexedSetter)

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

  test("andThen with Review") {
    (grate andThen review).review(9) shouldEqual 9
  }
}
