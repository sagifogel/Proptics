package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.empty.{empty => _empty}
import proptics.typeclass.Empty

trait EmptyTests[S] extends Laws {
  def empty(implicit ev: Empty[S], eqS: Eq[S], arbA: Arbitrary[S]): RuleSet =
    new SimpleRuleSet("Empty[S] empty", PrismTests(_empty[S]).prism.props: _*)
}

object EmptyTests {
  def apply[S]: EmptyTests[S] = new EmptyTests[S] {}
}
