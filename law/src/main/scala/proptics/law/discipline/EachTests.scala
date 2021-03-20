package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.each.{each => _each}
import proptics.typeclass.Each

trait EachTests[S, A] extends Laws {
  def each(implicit ev: Each[S, A], eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbA: Arbitrary[A], arbOp: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet("Each[S, A] each", TraversalTests(_each(ev)).traversal.props: _*)
}

object EachTests {
  def apply[S, A]: EachTests[S, A] = new EachTests[S, A] {}
}
