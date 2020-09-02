package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.Grate

trait GrateTests[S, A] extends Laws {
  def laws: GrateLaws[S, A]

  def grate(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "Grate",
      "identityLaws" -> forAll(laws.identityLaw _),
      "consistentFoci" -> forAll((s: S, f: A => A, g: A => A) => laws.consistentFoci(s, f, g)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
}

object GrateTests {
  def apply[S, A](_grate: Grate[S, A]): GrateTests[S, A] =
    new GrateTests[S, A] { def laws: GrateLaws[S, A] = GrateLaws[S, A](_grate) }
}
