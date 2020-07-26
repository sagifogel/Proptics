package proptics.law

import cats.Eq
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.IndexedTraversal_

object IndexedTraversalRules extends Laws {
  def apply[I: Arbitrary: Eq, S: Arbitrary: Eq, T: Arbitrary: Eq, A: Arbitrary: Eq](indexTraversal: IndexedTraversal_[I, S, S, A, A])(
      implicit ev1: Arbitrary[(I, A) => A],
      ev2: Arbitrary[Option[(I, A)]],
      ev3: Eq[Option[(I, A)]],
      ev4: Arbitrary[List[(I, A)]],
      ev5: Eq[List[(I, A)]]): RuleSet = {

    val laws = IndexedTraversalLaws(indexTraversal)

    new SimpleRuleSet(
      "IndexedTraversal",
      "respectPurity" -> forAll(laws.respectPurity[Option] _),
      "consistentFoci" -> forAll((s: S, f: (I, A) => A, g: (I, A) => A) => laws.consistentFoci(s, f, g)),
      "preview" -> forAll(laws.preview _),
      "viewAllSet" -> forAll((s: S, f: (I, A) => A) => laws.getSet(s, f)),
      "setSet" -> forAll((s: S, a: A) => laws.setSet(s, a)),
      "overIdentity" -> forAll(laws.overIdentity _),
      "composeOver" -> forAll((s: S, f: (I, A) => A, g: (I, A) => A) => laws.composeOver(s)(f)(g))
    )
  }
}
