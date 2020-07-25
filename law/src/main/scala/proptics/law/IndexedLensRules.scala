package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.IndexedLens

object IndexedLensRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq, I: Arbitrary: Eq](lens: IndexedLens[I, S, A])(implicit ev: Arbitrary[(I, A) => A]): RuleSet = {
    val laws = IndexedLensLaws(lens)

    new SimpleRuleSet(
      "IndexedLens",
      "setView" -> forAll(laws.setGet _),
      "viewSet" -> forAll((s: S, a: A) => laws.getSet(s, a)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: (I, A) => A, g: (I, A) => A) => laws.composeOver(s)(f)(g)),
      "composeSourceLens" -> forAll(laws.composeSourceLens _),
      "composeFocusLens" -> forAll((s: S, a: A) => laws.composeFocusLens(s, a))
    )
  }
}
