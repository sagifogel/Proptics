package proptics.law

import cats.Eq
import cats.laws.discipline.ProfunctorTests
import org.scalacheck.{Arbitrary, Cogen}
import proptics.profunctor.Cochoice

trait CochoiceTests[F[_, _]] extends ProfunctorTests[F] {
  def laws: CochoiceLaws[F]

  def cochoice[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](
      implicit
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
      EqFEitherABC: Eq[F[A, Either[B, C]]],
      EqFEitherACB: Eq[F[A, Either[C, B]]],
      LEqFEitherABC: Eq[F[Either[C, A], Either[C, B]]],
      REqFEitherABC: Eq[F[Either[A, C], Either[B, C]]],
      EitherAssociationACB: Eq[F[Either[Either[A, C], B], Either[Either[B, C], B]]],
      EitherAssociationBCA: Eq[F[Either[B, Either[C, A]], Either[B, Either[C, B]]]]): RuleSet =
    new DefaultRuleSet(
      name = "cochoice",
      parent = Some(profunctor[A, B, C, D, E, G])
    )
}

object CochoiceTests {
  def apply[F[_, _]: Cochoice]: CochoiceTests[F] =
    new CochoiceTests[F] { def laws: CochoiceLaws[F] = CochoiceLaws[F] }
}
