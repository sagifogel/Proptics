package optics

import cats.data.OneAnd
import cats.instances.function._

import proptics.specs.PropticsSuite
import proptics.{Setter, Setter_}

class SetterExamples extends PropticsSuite {
  test("create as Setter using the apply method") {
    val setter = Setter[Int, Int](f => i => f(i))

    assertResult(9)(setter.over(_ + 1)(8))
  }

  test("Setter from functor") {
    val setter = Setter.fromFunctor[List, Int]

    assertResult(List.range(2, 5))(setter.over(_ + 1)(List.range(1, 4)))
  }

  test("polymorphic Setter from Functor") {
    val setter: Setter_[List[Int], List[String], Int, String] =
      Setter_.fromFunctor[List, Int, String]
    val expected = List.range(2, 5).map(_.toString)

    assertResult(expected)(setter.over(i => (i + 1).toString)(List.range(1, 4)))
  }

  test("polymorphic Setter from Contravariant") {
    val fromContravariant =
      Setter_.fromContravariant[* => String, OneAnd[List, String], String]
    val head = fromContravariant.over(_.head)(_.toUpperCase)

    assertResult("HEAD")(head(OneAnd("head", List.empty)))
  }
}
