package proptics.law

import cats.Eq
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.APrism

object APrismRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](aPrism: APrism[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    val laws = APrismLaws(aPrism)

    new SimpleRuleSet(
      "Prism",
      "previewReview" -> forAll(laws.previewReview _),
      "viewOrModifyReview" -> forAll(laws.viewOrModifyReview _),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
  }
}
