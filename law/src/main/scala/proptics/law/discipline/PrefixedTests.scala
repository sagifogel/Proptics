package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.prefixed.{prefixed => pre}
import proptics.typeclass.Prefixed

trait PrefixedTests[S, T] extends Laws {
  def prefixed(
      implicit ev: Prefixed[S, T],
      eqS: Eq[S],
      eqT: Eq[T],
      arbS: Arbitrary[S],
      arbT: Arbitrary[T],
      arbOp: Arbitrary[T => T],
      arbOp2: Arbitrary[Option[T] => Option[T]]): RuleSet =
    new SimpleRuleSet("Prefixed[S, T] prefix ", PrismTests(pre(_: S)(ev)).prism.props: _*)
}

object PrefixedTests {
  def apply[S, T]: PrefixedTests[S, T] = new PrefixedTests[S, T] {}
}
