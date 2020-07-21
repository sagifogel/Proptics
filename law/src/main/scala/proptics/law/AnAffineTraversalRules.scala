package proptics.law

import cats.Eq
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.AnAffineTraversal

object AnAffineTraversalRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](anAffineTraversal: AnAffineTraversal[S, A])(implicit ev: Arbitrary[A => A]): RuleSet = {
    val laws = AnAffineTraversalLaws(anAffineTraversal)

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
}
