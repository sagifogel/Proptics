package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.nonEmptyCons.{head => neHead, nonEmptyCons => nec, tail => neTail}
import proptics.typeclass.NonEmptyCons

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
    new SimpleRuleSet("NonEmptyCons[S, H, T] nonEmptyCons ", IsoTests(nec(ev)).iso.props: _*)

  def head(implicit ev: NonEmptyCons[S, H, T], eqS: Eq[S], eqH: Eq[H], eqT: Eq[T], arbS: Arbitrary[S], arbH: Arbitrary[H], arbT: Arbitrary[T], arbH2H: Arbitrary[H => H]): RuleSet =
    new SimpleRuleSet("NonEmptyCons[S, H, T] head", LensTests(neHead(ev)).lens.props: _*)

  def tail(implicit ev: NonEmptyCons[S, H, T], eqS: Eq[S], eqH: Eq[H], eqT: Eq[T], arbS: Arbitrary[S], arbH: Arbitrary[H], arbT: Arbitrary[T], arbA2A: Arbitrary[T => T]): RuleSet =
    new SimpleRuleSet("NonEmptyCons[S, H, T] tail", LensTests(neTail(ev)).lens.props: _*)
}

object NonEmptyConsTests {
  def apply[S, H, T]: NonEmptyConsTests[S, H, T] = new NonEmptyConsTests[S, H, T] {}
}
