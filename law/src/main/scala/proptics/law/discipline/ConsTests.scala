package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.Cons
import proptics.instances.cons.{cons => cns, headOption => headOp, tailOption => tailOp}

trait ConsTests[S, A] extends Laws {
  def cons(implicit ev: Cons[S, A], eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbA: Arbitrary[A], arbOp: Arbitrary[((A, S)) => (A, S)]): RuleSet =
    new SimpleRuleSet("cons", PrismTests(cns(ev)).prism.props: _*)

  def headOption(implicit ev: Cons[S, A], eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbA: Arbitrary[A], arbA2A: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet("headOption", AffineTraversalTests(headOp(ev)).affineTraversal.props: _*)

  def tailOption(implicit ev: Cons[S, A], eqS: Eq[S], eqA: Eq[A], arbS: Arbitrary[S], arbA: Arbitrary[A], arbA2A: Arbitrary[S => S]): RuleSet =
    new SimpleRuleSet("tailOption", AffineTraversalTests(tailOp(ev)).affineTraversal.props: _*)
}

object ConsTests {
  @inline def apply[S, A]: ConsTests[S, A] = new ConsTests[S, A] {}
}
