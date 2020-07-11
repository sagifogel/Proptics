package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.AnIso

object AnIsoRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](anIso: AnIso[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    val laws = AnIsoLaws(anIso)

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
}
