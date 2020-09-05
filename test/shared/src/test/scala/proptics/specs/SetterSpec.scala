package proptics.specs

import cats.Show
import cats.instances.int._
import cats.instances.list._
import proptics.law.SetterTests
import proptics.specs.compose._
import proptics.specs.Whole._
import proptics.{Setter, Setter_}

class SetterSpec extends PropticsSuite {
  val fromFunctor: Setter[List[Int], Int] = Setter.fromFunctor[List, Int]
  val wholeSetter: Setter[Whole, Int] = Setter[Whole, Int](f => w => w.copy(part = f(w.part)))
  val fromContravariant: Setter_[Show[Int], Show[List[Int]], List[Int], Int] =
    Setter_.fromContravariant[Show, List[Int], Int]

  checkAll("Setter[Whole, Int] apply", SetterTests(wholeSetter).setter)
  checkAll("Setter[Int, Int] id", SetterTests(Setter.id[Int]).setter)
  checkAll("Setter[List[Int], Int] fromFunctor", SetterTests(fromFunctor).setter)
  checkAll("Setter[Int, Int] compose with Iso[Int, Int]", SetterTests(setter compose iso).setter)
  checkAll("Setter[Int, Int] compose with AnIso[Int, Int]", SetterTests(setter compose anIso).setter)
  checkAll("Setter[Int, Int] compose with Lens[Int, Int]", SetterTests(setter compose lens).setter)
  checkAll("Setter[Int, Int] compose with ALens[Int, Int]", SetterTests(setter compose aLens).setter)
  checkAll("Setter[Int, Int] compose with Prism[Int, Int]", SetterTests(setter compose prism).setter)
  checkAll("Setter[Int, Int] compose with APrism[Int, Int]", SetterTests(setter compose aPrism).setter)
  checkAll("Setter[Int, Int] compose with AffineTraversal[Int, Int]", SetterTests(setter compose affineTraversal).setter)
  checkAll("Setter[Int, Int] compose with AnAffineTraversal[Int, Int]", SetterTests(setter compose anAffineTraversal).setter)
  checkAll("Setter[Int, Int] compose with Traversal[Int, Int]", SetterTests(setter compose traversal).setter)
  checkAll("Setter[Int, Int] compose with ATraversal[Int, Int]", SetterTests(setter compose aTraversal).setter)
  checkAll("Setter[Int, Int] compose with Setter[Int, Int]", SetterTests(setter compose setter).setter)
  checkAll("Setter[Int, Int] compose with Grate[Int, Int]", SetterTests(setter compose grate).setter)

  test("set") {
    fromFunctor.set(9)(List(1)) shouldEqual List(9)
    fromContravariant.set(10)(Show.fromToString[Int]).show(list) shouldEqual 10.toString
    wholeSetter.set(9)(Whole(0)) shouldEqual whole9
  }

  test("over") {
    fromFunctor.over(_ + 1)(List.fill(1)(8)) shouldEqual List(9)
    fromContravariant.over(_.length)(Show.fromToString[Int]).show(list) shouldEqual list.length.toString
    wholeSetter.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
}
