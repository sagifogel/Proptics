package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import proptics.instances.Index
import proptics.instances.index.{index => idx}

trait IndexTests[S, I, A] extends Laws {
  def index(
      implicit
      ev: Index[S, I, A],
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbI: Arbitrary[I],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A],
      arbOp: Arbitrary[Option[A] => Option[A]]): RuleSet =
    new SimpleRuleSet("Index", AffineTraversalTests(idx(_: I)(ev)).affineTraversal.props: _*)
}

object IndexTests {
  def apply[S, I, A]: IndexTests[S, I, A] =
    new IndexTests[S, I, A] {}
}
