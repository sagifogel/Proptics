package proptics.law

import cats.Eq
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.IndexedTraversal

trait IndexedTraversalTests[I, S, A] extends Laws {
  def laws: IndexedTraversalLaws[I, S, A]

  def indexedTraversal(
      implicit
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbA: Arbitrary[A],
      arbIA2A: Arbitrary[(I, A) => A],
      arbOpIA: Arbitrary[Option[(I, A)]],
      eqOpIa: Eq[Option[(I, A)]],
      arbListIA: Arbitrary[List[(I, A)]],
      eqListIA: Eq[List[(I, A)]]): RuleSet =
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

object IndexedTraversalTests {
  def apply[I, S, A](_indexedTraversal: IndexedTraversal[I, S, A]): IndexedTraversalTests[I, S, A] =
    new IndexedTraversalTests[I, S, A] { def laws: IndexedTraversalLaws[I, S, A] = IndexedTraversalLaws[I, S, A](_indexedTraversal) }
}
