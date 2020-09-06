package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.Lens
import proptics.law.LensLaws

trait LensTests[S, I, A] extends Laws {
  def laws(i: I): LensLaws[S, A]

  def lens(implicit eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbI: Arbitrary[I], arbA: Arbitrary[A], arbAA: Arbitrary[A => A]): RuleSet =
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

object LensTests {
  def apply[S, A](_iso: Lens[S, A]): LensTests[S, Unit, A] = new LensTests[S, Unit, A] {
    def laws(unit: Unit): LensLaws[S, A] = LensLaws[S, A](_iso)
  }

  def apply[S, I, A](f: I => Lens[S, A]): LensTests[S, I, A] = new LensTests[S, I, A] {
    def laws(i: I): LensLaws[S, A] = LensLaws[S, A](f(i))
  }
}
