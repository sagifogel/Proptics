package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import proptics.instances.Index
import proptics.instances.index._

object IndexRules extends Laws {
  def apply[S: Eq: Arbitrary, I: Arbitrary, A: Eq: Arbitrary](implicit ev: Index[S, I, A], arbAA: Arbitrary[A => A], arbOp: Arbitrary[Option[A] => Option[A]]): RuleSet =
    new SimpleRuleSet("Index", AffineTraversalTests(index(_: I)(ev)).affineTraversal.props: _*)
}
