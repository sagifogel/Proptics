package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen}
import proptics.law.CochoiceLaws
import proptics.profunctor.Cochoice

trait CochoiceTests[F[_, _]] extends ProfunctorTests[F] {
  def laws: CochoiceLaws[F]

  def cochoice[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](
      implicit
      ArbFAB: Arbitrary[F[A, B]],
      ArbFCD: Arbitrary[F[C, D]],
      ArbEACEBC: Arbitrary[F[Either[A, C], Either[B, C]]],
      ArbECAECB: Arbitrary[F[Either[C, A], Either[C, B]]],
      ArbEEABCEEBCC: Arbitrary[F[Either[Either[A, B], C], Either[Either[B, C], C]]],
      ArbECEABECEBC: Arbitrary[F[Either[C, Either[A, B]], Either[C, Either[B, C]]]],
      CogenA: Cogen[A],
      CogenB: Cogen[B],
      CogenC: Cogen[C],
      CogenD: Cogen[D],
      CogenE: Cogen[E],
      EqFAB: Eq[F[A, B]],
      EqFBC: Eq[F[B, C]],
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
      parent = Some(profunctor[A, B, C, D, E, G]),
      "unleft unright consistent" -> forAll(laws.unleftUnrightConsistent[A, B, C] _),
      "unright unleft consistent" -> forAll(laws.unrightUnLeftConsistent[A, B, C] _),
      "unleft compose unleft dimap unleft consistent" -> forAll(laws.unleftComposeUnleftDimapConsistent[A, B, C] _),
      "unright compose unright dimap unright consistent" -> forAll(laws.unrightComposeUnrightDimapConsistent[A, B, C] _)
    )
}

object CochoiceTests {
  def apply[F[_, _]: Cochoice]: CochoiceTests[F] =
    new CochoiceTests[F] { def laws: CochoiceLaws[F] = CochoiceLaws[F] }
}
