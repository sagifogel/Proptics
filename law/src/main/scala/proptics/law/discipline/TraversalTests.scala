package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.Traversal
import proptics.law.TraversalLaws

trait TraversalTests[S, A] extends Laws {
  def laws: TraversalLaws[S, A]

  def traversal(implicit eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbA: Arbitrary[A], arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "Traversal",
      "respectPurity" -> forAll(laws.respectPurity[Option] _),
      "consistentFoci" -> forAll((s: S, f: A => A, g: A => A) => laws.consistentFoci(s, f, g)),
      "preview" -> forAll(laws.preview _),
      "setGet" -> forAll((s: S, f: A => A) => laws.getSet(s, f)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
}

object TraversalTests {
  def apply[S, A](_traversal: Traversal[S, A]): TraversalTests[S, A] =
    new TraversalTests[S, A] { def laws: TraversalLaws[S, A] = TraversalLaws[S, A](_traversal) }
}
