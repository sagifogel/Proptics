package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.AnIndexedLens
import proptics.law.AnIndexedLensLaws

trait AnIndexedLensTests[I, S, A] extends Laws {
  def laws: AnIndexedLensLaws[I, S, A]

  def anIndexedLens(implicit eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbA: Arbitrary[A], arbAA: Arbitrary[A => A], arbIAA: Arbitrary[(I, A) => A]): RuleSet =
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

object AnIndexedLensTests {
  def apply[I, S, A](_anIndexedLens: AnIndexedLens[I, S, A]): AnIndexedLensTests[I, S, A] =
    new AnIndexedLensTests[I, S, A] { def laws: AnIndexedLensLaws[I, S, A] = AnIndexedLensLaws(_anIndexedLens) }
}
