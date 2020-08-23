package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.IndexedSetter

object IndexedSetterRules extends Laws {
  def apply[I: Arbitrary: Eq, S: Arbitrary: Eq, A: Arbitrary: Eq](indexedSetter: IndexedSetter[I, S, A])(implicit ev1: Arbitrary[(I, A) => A]): RuleSet = {
    val laws = IndexedSetterLaws(indexedSetter)

    new SimpleRuleSet(
      "IndexedSetter",
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "setTwiceSet" -> forAll((s: S, a: A, b: A) => laws.setASetB(s, a, b)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: (I, A) => A, g: (I, A) => A) => laws.composeOver(s)(f)(g))
    )
  }
}
