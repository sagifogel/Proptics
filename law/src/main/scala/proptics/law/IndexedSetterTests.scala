package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.IndexedSetter

trait IndexedSetterTests[I, S, A] extends Laws {
  def laws: IndexedSetterLaws[I, S, A]

  def indexedSetter(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A],
      arbIAA: Arbitrary[(I, A) => A]): RuleSet =
    new SimpleRuleSet(
      "IndexedSetter",
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "setTwiceSet" -> forAll((s: S, a: A, b: A) => laws.setASetB(s, a, b)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: (I, A) => A, g: (I, A) => A) => laws.composeOver(s)(f)(g))
    )
}

object IndexedSetterTests {
  def apply[I, S, A](_indexedSetter: IndexedSetter[I, S, A]): IndexedSetterTests[I, S, A] =
    new IndexedSetterTests[I, S, A] { def laws: IndexedSetterLaws[I, S, A] = IndexedSetterLaws[I, S, A](_indexedSetter) }
}
