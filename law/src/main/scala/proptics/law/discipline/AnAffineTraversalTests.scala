package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import proptics.AnAffineTraversal
import proptics.law.AnAffineTraversalLaws

trait AnAffineTraversalTests[S, A] extends Laws {
  def laws: AnAffineTraversalLaws[S, A]

  def anAffineTraversal(implicit eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbA: Arbitrary[A], arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "AffineTraversal",
      "respectPurity" -> forAll(laws.respectPurity[Option] _),
      "consistentFoci" -> forAll((s: S, f: A => A, g: A => A) => laws.consistentFoci(s, f, g)),
      "viewOrModifySet" -> forAll(laws.getSet _),
      "previewSet" -> forAll((s: S, a: A) => laws.previewSet(s, a)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: A => A, g: A => A) => laws.composeOver(s)(f)(g))
    )
}

object AnAffineTraversalTests {
  def apply[S, A](_anAffineTraversal: AnAffineTraversal[S, A]): AnAffineTraversalTests[S, A] =
    new AnAffineTraversalTests[S, A] { def laws: AnAffineTraversalLaws[S, A] = AnAffineTraversalLaws[S, A](_anAffineTraversal) }
}
