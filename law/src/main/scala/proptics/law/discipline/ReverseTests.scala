package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.reverse.{reverse => revrse}
import proptics.typeclass.Reverse

trait ReverseTests[S, T] extends Laws {
  def reverse(implicit ev: Reverse[S, T], eqS: Eq[S], eqT: Eq[T], arbS: Arbitrary[S], arbT: Arbitrary[T], arbOp: Arbitrary[T => T]): RuleSet =
    new SimpleRuleSet("Reverse[S, T] reverse ", IsoTests(revrse(ev)).iso.props: _*)
}

object ReverseTests {
  def apply[S, T]: ReverseTests[S, T] = new ReverseTests[S, T] {}
}
