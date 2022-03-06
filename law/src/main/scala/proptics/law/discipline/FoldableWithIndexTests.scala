package proptics.law.discipline

import cats.Eq
import cats.kernel.CommutativeMonoid
import cats.laws.discipline._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline.Laws

import proptics.indices.FoldableWithIndex
import proptics.law.FoldableWithIndexLaws

trait FoldableWithIndexTests[F[_], I] extends Laws {
  def laws: FoldableWithIndexLaws[F, I]

  def foldableWithIndex[A: Arbitrary, B: Arbitrary, C: Arbitrary](
      implicit ArbFA: Arbitrary[F[A]],
      ArbI: Arbitrary[I],
      ArbAI2B: Arbitrary[(A, I) => B],
      CogenA: Cogen[A],
      CogenB: Cogen[B],
      A: CommutativeMonoid[A],
      B: CommutativeMonoid[B],
      EqA: Eq[A],
      EqB: Eq[B],
      EqFA: Eq[F[A]]): RuleSet =
    new SimpleRuleSet(
      name = "FoldableWithIndex",
      "foldLeft consistent with foldMap" -> forAll(laws.leftFoldConsistentWithFoldMap[A, B] _),
      "foldRight consistent with foldMap" -> forAll(laws.rightFoldConsistentWithFoldMap[A, B] _),
      "foldRight is lazy" -> forAll(laws.foldRightLazy[A] _),
      "foldLeftWithIndex consistent with foldMapWithIndex" ->
        forAll(laws.leftFoldWithIndexConsistentWithFoldMapWithIndex[A, B] _),
      "foldRightWithIndex consistent with foldMapWithIndex" ->
        forAll(laws.rightFoldWithIndexConsistentWithFoldMapWithIndex[A, B] _),
      "foldRightWithIndex is lazy" -> forAll(laws.foldRightWithIndexLazy[A] _)
    )
}

object FoldableWithIndexTests {
  def apply[F[_], I](implicit ev: FoldableWithIndex[F, I]): FoldableWithIndexTests[F, I] =
    new FoldableWithIndexTests[F, I] {
      def laws: FoldableWithIndexLaws[F, I] = FoldableWithIndexLaws[F, I]
    }
}
