package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.Lens

object LensRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](lens: Lens[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    val laws = LensLaws(lens)

    new SimpleRuleSet(
      "Lens",
      "getSet" -> forAll(laws.getSet _),
      "setGet" -> forAll((s: S, a: A) => laws.setGet(s, a)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g)),
      "composeSourceIso" -> forAll(laws.composeSourceIso _),
      "composeFocusIso" -> forAll((s: S, a: A) => laws.composeFocusIso(s, a))
    )
  }
}
