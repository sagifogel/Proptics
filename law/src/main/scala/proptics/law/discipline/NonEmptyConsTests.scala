package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.NonEmptyCons
import proptics.instances.nonEmptyCons.{head => headOp, nonEmptyCons => cons, tail => tailOp}

trait NonEmptyConsTests[S, H, T] extends Laws {
  def nonEmptyCons(
      implicit ev: NonEmptyCons[S, H, T],
      eqS: Eq[S],
      eqH: Eq[H],
      eqT: Eq[T],
      arbS: Arbitrary[S],
      arbH: Arbitrary[H],
      arbT: Arbitrary[T],
      arbOp: Arbitrary[((H, T)) => (H, T)]): RuleSet =
    new SimpleRuleSet("nonEmptyCons", IsoTests(cons(ev)).iso.props: _*)

  def headOption(implicit ev: NonEmptyCons[S, H, T], eqS: Eq[S], eqH: Eq[H], eqT: Eq[T], arbS: Arbitrary[S], arbH: Arbitrary[H], arbT: Arbitrary[T], arbH2H: Arbitrary[H => H])
      : RuleSet =
    new SimpleRuleSet("headOption", LensTests(headOp(ev)).lens.props: _*)

  def tailOption(implicit ev: NonEmptyCons[S, H, T], eqS: Eq[S], eqH: Eq[H], eqT: Eq[T], arbS: Arbitrary[S], arbH: Arbitrary[H], arbT: Arbitrary[T], arbA2A: Arbitrary[T => T])
      : RuleSet =
    new SimpleRuleSet("tailOption", LensTests(tailOp(ev)).lens.props: _*)
}

object NonEmptyConsTests {
  def apply[S, H, T]: NonEmptyConsTests[S, H, T] = new NonEmptyConsTests[S, H, T] {}
}
