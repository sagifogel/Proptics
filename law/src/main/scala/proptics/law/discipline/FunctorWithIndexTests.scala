package proptics.law.discipline

import cats.Eq
import cats.laws.discipline.{FunctorTests, _}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen}

import proptics.indices.FunctorWithIndex
import proptics.law.FunctorWithIndexLaws

trait FunctorWithIndexTests[F[_], I] extends FunctorTests[F] {
  def laws: FunctorWithIndexLaws[F, I]

  def functorWithIndex[A: Arbitrary, B: Arbitrary, C: Arbitrary](
      implicit ArbFA: Arbitrary[F[A]],
      ArbI: Arbitrary[I],
      ArbAI2B: Arbitrary[(A, I) => B],
      ArbBI2C: Arbitrary[(B, I) => C],
      CogenA: Cogen[A],
      CogenB: Cogen[B],
      CogenC: Cogen[C],
      EqFA: Eq[F[A]],
      EqFC: Eq[F[C]]): RuleSet =
    new DefaultRuleSet(
      name = "functorWithIndex",
      parent = Some(functor[A, B, C]),
      "identity" -> forAll(laws.functorWithIndexIdentity[A] _),
      "composition" -> forAll(laws.functorWithIndexComposition[A, B, C] _)
    )
}

object FunctorWithIndexTests {
  def apply[F[_], I](implicit ev: FunctorWithIndex[F, I]): FunctorWithIndexTests[F, I] =
    new FunctorWithIndexTests[F, I] {
      def laws: FunctorWithIndexLaws[F, I] = FunctorWithIndexLaws[F, I]
    }
}
