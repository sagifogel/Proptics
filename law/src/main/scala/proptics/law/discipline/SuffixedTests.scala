package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.suffixed.{suffixed => suff}
import proptics.typeclass.Suffixed

trait SuffixedTests[S, T] extends Laws {
  def suffixed(
      implicit ev: Suffixed[S, T],
      eqS: Eq[S],
      eqT: Eq[T],
      arbS: Arbitrary[S],
      arbT: Arbitrary[T],
      arbOp: Arbitrary[T => T],
      arbOp2: Arbitrary[Option[T] => Option[T]]): RuleSet =
    new SimpleRuleSet("Suffixed[S, T] suffix ", PrismTests(suff(_: S)(ev)).prism.props: _*)
}

object SuffixedTests {
  def apply[S, T]: SuffixedTests[S, T] = new SuffixedTests[S, T] {}
}
