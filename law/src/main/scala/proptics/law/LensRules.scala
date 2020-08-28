package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.Lens
import Function.const

object LensRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](lens: Lens[S, A])(implicit ev: Arbitrary[A => A]): RuleSet =
    apply[S, A, Unit](const(lens))

  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq, I: Arbitrary](f: I => Lens[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    def laws(i: I): LensLaws[S, A] = LensLaws(f(i))

    new SimpleRuleSet(
      "Lens",
      "setView" -> forAll((i: I, s: S) => laws(i).setGet(s)),
      "viewSet" -> forAll((i: I, s: S, a: A) => laws(i).getSet(s, a)),
      "setSet" -> forAll((i: I, s: S, a: A) => laws(i).setSet(s, a)),
      "overIdentity" -> forAll((i: I, s: S) => laws(i).overIdentity(s)),
      "composeOver" -> forAll((i: I, s: S, f: A => A, g: A => A) => laws(i).composeOver(s)(f)(g)),
      "composeSourceLens" -> forAll((i: I, s: S) => laws(i).composeSourceLens(s)),
      "composeFocusLens" -> forAll((i: I, s: S, a: A) => laws(i).composeFocusLens(s, a))
    )
  }
}
