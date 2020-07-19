package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.Setter

object SetterRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](setter: Setter[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    val laws = SetterLaws(setter)

    new SimpleRuleSet(
      "Setter",
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "setTwiceSet" -> forAll((s: S, a: A, b: A) => laws.setASetB(s, a, b)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
  }
}
