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

  checkAll("Setter apply", SetterRules(wholeSetter))
  checkAll("Setter fromFunctor", SetterRules(fromFunctor))

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

  checkAll("compose with Iso", SetterRules(setter compose iso))
  checkAll("compose with AnIso", SetterRules(setter compose anIso))
  checkAll("compose with Lens", SetterRules(setter compose lens))
  checkAll("compose with ALens", SetterRules(setter compose aLens))
  checkAll("compose with Prism", SetterRules(setter compose traversal))
  checkAll("compose with APrism", SetterRules(setter compose aPrism))
  checkAll("compose with AffineTraversal", SetterRules(setter compose traversal))
  checkAll("compose with AnAffineTraversal", SetterRules(setter compose anAffineTraversal))
  checkAll("compose with Traversal", SetterRules(setter compose traversal))
  checkAll("compose with ATraversal", SetterRules(setter compose aTraversal))
  checkAll("compose with Setter", SetterRules(setter compose setter))
  checkAll("compose with Grate", SetterRules(setter compose grate))
}
