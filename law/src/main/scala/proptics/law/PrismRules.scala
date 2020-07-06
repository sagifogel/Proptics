package proptics.law

import cats.Eq
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.Prism

object PrismRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](prism: Prism[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    val laws = PrismLaws(prism)

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
