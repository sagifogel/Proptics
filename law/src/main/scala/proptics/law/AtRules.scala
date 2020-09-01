package proptics.law

import cats.Eq
import cats.instances.option._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import proptics.instances.At
import proptics.instances.at._

object AtRules extends Laws {
  def apply[S: Eq: Arbitrary, I: Arbitrary, A: Eq: Arbitrary](implicit ev: At[S, I, A], arbAA: Arbitrary[A => A], arbOp: Arbitrary[Option[A] => Option[A]]): RuleSet =
    new SimpleRuleSet("At", LensTests(at(_: I)(ev)).lens.props: _*)
}
