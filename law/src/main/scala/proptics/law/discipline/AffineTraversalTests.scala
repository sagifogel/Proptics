package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.AffineTraversal
import proptics.law.AffineTraversalLaws

trait AffineTraversalTests[S, I, A] extends Laws {
  def laws(i: I): AffineTraversalLaws[S, A]
  def affineTraversal(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbI: Arbitrary[I],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet(
      "AffineTraversal",
      "respectPurity" -> forAll((i: I, s: S) => laws(i).respectPurity[Option](s)),
      "consistentFoci" -> forAll((i: I, s: S, f: A => A, g: A => A) => laws(i).consistentFoci(s, f, g)),
      "viewOrModifySet" -> forAll((i: I, s: S) => laws(i).getSet(s)),
      "previewSet" -> forAll((i: I, s: S, a: A) => laws(i).previewSet(s, a)),
      "setSet" -> forAll((i: I, s: S, a: A) => laws(i).setSet(s, a)),
      "overIdentity" -> forAll((i: I, s: S) => laws(i).overIdentity(s)),
      "composeOver" -> forAll((i: I, s: S, f: A => A, g: A => A) => laws(i).composeOver(s)(f)(g))
    )
}

object AffineTraversalTests {
  def apply[S, A](_affineTraversal: AffineTraversal[S, A]): AffineTraversalTests[S, Unit, A] =
    new AffineTraversalTests[S, Unit, A] {
      def laws(unit: Unit): AffineTraversalLaws[S, A] = AffineTraversalLaws[S, A](_affineTraversal)
    }

  def apply[S, I, A](f: I => AffineTraversal[S, A]): AffineTraversalTests[S, I, A] = new AffineTraversalTests[S, I, A] {
    def laws(i: I): AffineTraversalLaws[S, A] = AffineTraversalLaws[S, A](f(i))
  }
}
