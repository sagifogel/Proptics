package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.ALens

trait ALensTests[S, A] extends Laws {
  def laws: ALensLaws[S, A]

  def aLens(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "ALens",
      "setGet" -> forAll(laws.setGet _),
      "getSet" -> forAll((s: S, a: A) => laws.getSet(s, a)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g)),
      "composeSourceLens" -> forAll(laws.composeSourceLens _),
      "composeFocusLens" -> forAll((s: S, a: A) => laws.composeFocusLens(s, a))
    )
}

object ALensTests {
  def apply[S, A](_aLens: ALens[S, A]): ALensTests[S, A] =
    new ALensTests[S, A] { def laws: ALensLaws[S, A] = ALensLaws(_aLens) }
}
