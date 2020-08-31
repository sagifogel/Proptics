package proptics.law

import cats.Eq
import cats.laws.discipline.ProfunctorTests
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._
import proptics.profunctor.Choice

trait ChoiceTests[F[_, _]] extends ProfunctorTests[F] {
  def laws: ChoiceLaws[F]

  def choice[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](
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
      name = "choice",
      parent = Some(profunctor[A, B, C, D, E, G]),
      "right left consistent" -> forAll(laws.rightLeftConsistent[A, B, C] _),
      "left right consistent" -> forAll(laws.leftRightConsistent[A, B, C] _),
      "left rmap lmap consistent" -> forAll(laws.leftRmapLmapConsistent[A, B, C] _),
      "right lmap rmap consistent" -> forAll(laws.rightLmapRmapConsistent[A, B, C] _),
      "left compose left dimap consistent" -> forAll(laws.leftComposeLeftDimapConsistent[A, B, C] _),
      "right compose right dimap consistent" -> forAll(laws.rightComposeRightDimapConsistent[A, B, C] _)
    )
}

object ChoiceTests {
  def apply[F[_, _]: Choice]: ChoiceTests[F] =
    new ChoiceTests[F] { def laws: ChoiceLaws[F] = ChoiceLaws[F] }
}
