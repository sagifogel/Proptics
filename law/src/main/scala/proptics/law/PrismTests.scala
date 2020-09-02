package proptics.law

import cats.Eq
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.Prism

trait PrismTests[S, A] extends Laws {
  def laws: PrismLaws[S, A]

  def prism(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "Prism",
      "previewReview" -> forAll(laws.previewReview _),
      "viewOrModifyReview" -> forAll(laws.viewOrModifyReview _),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
}

object PrismTests {
  def apply[S, A](_prism: Prism[S, A]): PrismTests[S, A] =
    new PrismTests[S, A] { def laws: PrismLaws[S, A] = PrismLaws(_prism) }
}
