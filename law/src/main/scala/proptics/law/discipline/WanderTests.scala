package proptics.law.discipline

import cats.Eq
import cats.laws.discipline.StrongTests
import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline.Laws
import proptics.profunctor.Wander
import proptics.law.WanderLaws

trait WanderTests[F[_, _]] extends Laws {
  def laws: WanderLaws[F]

  def wander[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
      wander: Wander[F],
      ArbFAB: Arbitrary[F[A, B]],
      ArbFCD: Arbitrary[F[C, D]],
      CogenA: Cogen[A],
      CogenB: Cogen[B],
      CogenC: Cogen[C],
      CogenD: Cogen[D],
      CogenE: Cogen[E],
      EqFAB: Eq[F[A, B]],
      EqFAD: Eq[F[A, D]],
      EqFAG: Eq[F[A, G]],
      EqFACB: Eq[F[(A, C), B]],
      EqFCAB: Eq[F[(C, A), B]],
      EqFACBC: Eq[F[(A, C), (B, C)]],
      EqFDADB: Eq[F[(D, A), (D, B)]],
      EqFACBD: Eq[F[(A, C), (B, D)]],
      EqFCADB: Eq[F[(C, A), (D, B)]],
      EqFEitherABC: Eq[F[A, Either[B, C]]],
      EqFEitherACB: Eq[F[A, Either[C, B]]],
      EqFACDBCD: Eq[F[((A, C), D), ((B, C), D)]],
      EqFDCADCB: Eq[F[(D, (C, A)), (D, (C, B))]],
      LEqFEitherABC: Eq[F[Either[C, A], Either[C, B]]],
      REqFEitherABC: Eq[F[Either[A, C], Either[B, C]]],
      EitherAssociationACB: Eq[F[Either[Either[A, C], B], Either[Either[B, C], B]]],
      EitherAssociationBCA: Eq[F[Either[B, Either[C, A]], Either[B, Either[C, B]]]]): RuleSet =
    new SimpleRuleSet(
      name = "wander",
      ChoiceTests(wander).choice[A, B, C, D, E, G].props ++ StrongTests(wander).strong[A, B, C, D, E, G].props: _*
    )
}

object WanderTests {
  def apply[F[_, _]](implicit ev: Wander[F]): WanderTests[F] =
    new WanderTests[F] { def laws: WanderLaws[F] = WanderLaws[F] }
}
