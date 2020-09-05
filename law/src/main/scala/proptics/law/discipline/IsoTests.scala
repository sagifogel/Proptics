package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.Iso
import proptics.law.IsoLaws

trait IsoTests[S, A] extends Laws {
  def laws: IsoLaws[S, A]

  def iso(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "Iso",
      "sourceReversibility" -> forAll(laws.sourceReversibility _),
      "focusReversibility" -> forAll(laws.focusReversibility _),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g)),
      "composeOver" -> forAll(laws.composeFocusIso _),
      "composeSourceIso" -> forAll(laws.composeSourceIso _)
    )
}

object IsoTests {
  def apply[S, A](_iso: Iso[S, A]): IsoTests[S, A] = new IsoTests[S, A] {
    def laws: IsoLaws[S, A] = IsoLaws[S, A](_iso)
  }
}
