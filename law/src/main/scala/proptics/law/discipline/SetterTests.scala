package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.Setter
import proptics.law.SetterLaws

trait SetterTests[S, A] extends Laws {
  def laws: SetterLaws[S, A]

  def setter(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "Setter",
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "setTwiceSet" -> forAll((s: S, a: A, b: A) => laws.setASetB(s, a, b)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
}

object SetterTests {
  def apply[S, A](_setter: Setter[S, A]): SetterTests[S, A] =
    new SetterTests[S, A] { def laws: SetterLaws[S, A] = SetterLaws[S, A](_setter) }
}
