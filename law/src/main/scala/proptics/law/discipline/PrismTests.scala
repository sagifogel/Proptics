package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import proptics.Prism
import proptics.law.PrismLaws

trait PrismTests[S, T, A] extends Laws {
  def laws(t: T): PrismLaws[S, A]

  def prism(implicit eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbT: Arbitrary[T], arbA: Arbitrary[A], arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "Prism",
      "previewReview" -> forAll((t: T, a: A) => laws(t).previewReview(a)),
      "viewOrModifyReview" -> forAll((s: S, t: T) => laws(t).viewOrModifyReview(s)),
      "setSet" -> forAll((s: S, t: T, a: A) => laws(t).setSet(s, a)),
      "overIdentity" -> forAll((s: S, t: T) => laws(t).overIdentity(s)),
      "composeOver" -> forAll((s: S, t: T, f: A => A, g: A => A) => laws(t).composeOver(s)(f)(g))
    )
}

object PrismTests {
  def apply[S, A](_prism: Prism[S, A]): PrismTests[S, Unit, A] = new PrismTests[S, Unit, A] {
    def laws(unit: Unit): PrismLaws[S, A] = PrismLaws[S, A](_prism)
  }

  def apply[S, A](f: S => Prism[S, A]): PrismTests[S, S, A] = new PrismTests[S, S, A] {
    def laws(s: S): PrismLaws[S, A] = PrismLaws(f(s))
  }
}
