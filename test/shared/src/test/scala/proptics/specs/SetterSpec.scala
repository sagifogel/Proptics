package proptics.specs

import cats.Show
import cats.instances.int._
import cats.instances.list._
import proptics.{Setter, Setter_}
import proptics.law.SetterRules
import proptics.specs.Whole._

class SetterSpec extends PropticsSuite {
  val fromFunctor: Setter[List[Int], Int] = Setter.fromFunctor[List, Int]
  val setter: Setter[Whole, Int] = Setter[Whole, Int](f => w => w.copy(part = f(w.part)))
  val fromContravariant: Setter_[Show[Int], Show[List[Int]], List[Int], Int] =
    Setter_.fromContravariant[Show, List[Int], Int]

  checkAll("Setter apply", SetterRules(setter))
  checkAll("Setter fromFunctor", SetterRules(fromFunctor))

  test("set") {
    fromFunctor.set(9)(List(1)) shouldEqual List(9)
    fromContravariant.set(10)(Show.fromToString[Int]).show(list) shouldEqual 10.toString
    setter.set(9)(Whole(0)) shouldEqual whole9
  }

  test("over") {
    fromFunctor.over(_ + 1)(List.fill(1)(8)) shouldEqual List(9)
    fromContravariant.over(_.length)(Show.fromToString[Int]).show(list) shouldEqual list.length.toString
    setter.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
}
