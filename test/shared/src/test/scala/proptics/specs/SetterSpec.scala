package proptics.specs

import cats.Show
import cats.instances.int._
import cats.instances.list._
import proptics.law.SetterRules
import proptics.specs.Compose._
import proptics.specs.Whole._
import proptics.{Setter, Setter_}

class SetterSpec extends PropticsSuite {
  val fromFunctor: Setter[List[Int], Int] = Setter.fromFunctor[List, Int]
  val wholeSetter: Setter[Whole, Int] = Setter[Whole, Int](f => w => w.copy(part = f(w.part)))
  val fromContravariant: Setter_[Show[Int], Show[List[Int]], List[Int], Int] =
    Setter_.fromContravariant[Show, List[Int], Int]

  checkAll("Setter[Whole, Int] apply", SetterRules(wholeSetter))
  checkAll("Setter[Int, Int] id", SetterRules(Setter.id[Int]))
  checkAll("Setter[List[Int], Int] fromFunctor", SetterRules(fromFunctor))
  checkAll("Setter[Int, Int] compose with Iso[Int, Int]", SetterRules(setter compose iso))
  checkAll("Setter[Int, Int] compose with AnIso[Int, Int]", SetterRules(setter compose anIso))
  checkAll("Setter[Int, Int] compose with Lens[Int, Int]", SetterRules(setter compose lens))
  checkAll("Setter[Int, Int] compose with ALens[Int, Int]", SetterRules(setter compose aLens))
  checkAll("Setter[Int, Int] compose with Prism[Int, Int]", SetterRules(setter compose prism))
  checkAll("Setter[Int, Int] compose with APrism[Int, Int]", SetterRules(setter compose aPrism))
  checkAll("Setter[Int, Int] compose with AffineTraversal[Int, Int]", SetterRules(setter compose affineTraversal))
  checkAll("Setter[Int, Int] compose with AnAffineTraversal[Int, Int]", SetterRules(setter compose anAffineTraversal))
  checkAll("Setter[Int, Int] compose with Traversal[Int, Int]", SetterRules(setter compose traversal))
  checkAll("Setter[Int, Int] compose with ATraversal[Int, Int]", SetterRules(setter compose aTraversal))
  checkAll("Setter[Int, Int] compose with Setter[Int, Int]", SetterRules(setter compose setter))
  checkAll("Setter[Int, Int] compose with Grate[Int, Int]", SetterRules(setter compose grate))

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
