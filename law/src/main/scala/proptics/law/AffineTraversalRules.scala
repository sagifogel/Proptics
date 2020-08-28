package proptics.law

import cats.Eq
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.AffineTraversal
import Function.const

object AffineTraversalRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](affineTraversal: AffineTraversal[S, A])(implicit ev: Arbitrary[A => A]): RuleSet =
    AffineTraversalRules[S, Unit, A](const(affineTraversal))

  def apply[S: Arbitrary: Eq, I: Arbitrary, A: Arbitrary: Eq](f: I => AffineTraversal[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    def laws(i: I): AffineTraversalLaws[S, A] = AffineTraversalLaws(f(i))

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
}
