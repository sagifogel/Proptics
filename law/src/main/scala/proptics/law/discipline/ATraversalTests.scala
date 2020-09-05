package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.ATraversal
import proptics.law.ATraversalLaws

trait ATraversalTests[S, A] extends Laws {
  def laws: ATraversalLaws[S, A]

  def aTraversal(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "ATraversal",
      "respectPurity" -> forAll(laws.respectPurity _),
      "consistentFoci" -> forAll((s: S, f: A => A, g: A => A) => laws.consistentFoci(s, f, g)),
      "preview" -> forAll(laws.preview _),
      "setGet" -> forAll((s: S, f: A => A) => laws.setGet(s, f)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
}

object ATraversalTests {
  def apply[S, A](_aTraversal: ATraversal[S, A]): ATraversalTests[S, A] =
    new ATraversalTests[S, A] { def laws: ATraversalLaws[S, A] = ATraversalLaws[S, A](_aTraversal) }
}
