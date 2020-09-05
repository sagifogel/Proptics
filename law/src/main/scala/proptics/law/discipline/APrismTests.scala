package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.APrism
import proptics.law.APrismLaws

trait APrismTests[S, A] extends Laws {
  def laws: APrismLaws[S, A]

  def aPrism(
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

object APrismTests {
  def apply[S, A](_aPrism: APrism[S, A]): APrismTests[S, A] =
    new APrismTests[S, A] { def laws: APrismLaws[S, A] = APrismLaws[S, A](_aPrism) }
}
