package proptics.law.discipline

import cats.kernel.CommutativeMonoid
import cats.laws.discipline._
import cats.{CommutativeApplicative, Eq}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen, Prop}

import proptics.indices.TraverseWithIndex
import proptics.law.TraverseWithIndexLaws

trait TraverseWithIndexTests[F[_], I] extends FunctorWithIndexTests[F, I] with FoldableWithIndexTests[F, I] {
  def laws: TraverseWithIndexLaws[F, I]

  def traverseWithIndex[A: Arbitrary, B: Arbitrary, C: Arbitrary, M: Arbitrary, X[_]: CommutativeApplicative, Y[_]: CommutativeApplicative](
      implicit ArbI: Arbitrary[I],
      ArbFA: Arbitrary[F[A]],
      ArbFB: Arbitrary[F[B]],
      ArbXB: Arbitrary[X[B]],
      ArbXM: Arbitrary[X[M]],
      ArbYB: Arbitrary[Y[B]],
      ArbYC: Arbitrary[Y[C]],
      ArbYM: Arbitrary[Y[M]],
      ArbFXM: Arbitrary[F[X[M]]],
      ArbAI2B: Arbitrary[(A, I) => B],
      ArbBI2C: Arbitrary[(B, I) => C],
      CogenA: Cogen[A],
      CogenB: Cogen[B],
      CogenC: Cogen[C],
      CogenM: Cogen[M],
      M: CommutativeMonoid[M],
      MA: CommutativeMonoid[A],
      MB: CommutativeMonoid[B],
      EqFA: Eq[F[A]],
      EqFB: Eq[F[B]],
      EqFC: Eq[F[C]],
      EqM: Eq[M],
      EqA: Eq[A],
      EqB: Eq[B],
      EqXYFC: Eq[X[Y[F[C]]]],
      EqXFB: Eq[X[F[B]]],
      EqYFB: Eq[Y[F[B]]],
      EqXFM: Eq[X[F[M]]],
      EqYFM: Eq[Y[F[M]]],
      EqOptionA: Eq[Option[A]]): RuleSet =
    new RuleSet {
      def name: String = "TraverseWithIndex"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = List(functorWithIndex[A, B, C], foldableWithIndex[A, B, C])
      def props: Seq[(String, Prop)] =
        Seq(
          "traverseWithIndex traverseWithIndexIdentity" -> forAll(laws.traverseWithIndexIdentity[A, B] _),
          "traverseWithIndex traverseWithIndexComposition" -> forAll(laws.traverseWithIndexComposition[A, B, C] _)
        )
    }
}

object TraverseWithIndexTests {
  def apply[F[_], I](implicit ev: TraverseWithIndex[F, I]): TraverseWithIndexTests[F, I] =
    new TraverseWithIndexTests[F, I] {
      def laws: TraverseWithIndexLaws[F, I] = TraverseWithIndexLaws[F, I]
    }
}
