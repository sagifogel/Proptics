package proptics.law

import cats.Eq
import cats.instances.list._
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline._
import proptics.ATraversal

object ATraversalRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](
      aTraversal: ATraversal[S, A])(implicit ev0: Arbitrary[A => A], ev1: Arbitrary[Option[S]], ev2: Arbitrary[List[A]], ev3: Arbitrary[Option[A]]): RuleSet = {
    val laws = ATraversalLaws(aTraversal)

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
}
