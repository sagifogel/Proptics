package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.AnIso

trait AnIsoTests[S, A] extends Laws {
  def laws: AnIsoLaws[S, A]

  def anIso(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "AnIso",
      "sourceReversibility" -> forAll(laws.sourceReversibility _),
      "focusReversibility" -> forAll(laws.focusReversibility _),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g)),
      "composeOver" -> forAll(laws.composeFocusIso _),
      "composeSourceIso" -> forAll(laws.composeSourceIso _)
    )
}

object AnIsoTests {
  def apply[S, A](_anIso: AnIso[S, A]): AnIsoTests[S, A] = new AnIsoTests[S, A] {
    def laws: AnIsoLaws[S, A] = AnIsoLaws[S, A](_anIso)
  }
}
