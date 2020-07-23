package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.Grate

object GrateRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](grate: Grate[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    val laws: GrateLaws[S, A] = GrateLaws(grate)

    new SimpleRuleSet(
      "Grate",
      "identityLaws" -> forAll(laws.identityLaw _),
      "consistentFoci" -> forAll((s: S, f: A => A, g: A => A) => laws.consistentFoci(s, f, g)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
  }
}
