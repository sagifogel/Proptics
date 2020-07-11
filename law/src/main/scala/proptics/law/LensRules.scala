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
      "setGet" -> forAll(laws.setGet _),
      "getSet" -> forAll((s: S, a: A) => laws.getSet(s, a)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g)),
      "composeSourceLens" -> forAll(laws.composeSourceLens _),
      "composeFocusLens" -> forAll((s: S, a: A) => laws.composeFocusLens(s, a))
    )
  }
}
