package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.field3.{third => thrd}
import proptics.typeclass.Field3

trait Field3Tests[A, B, C] extends Laws {
  def third(implicit ev: Field3[(A, B, C), C], eqA: Eq[A], eqB: Eq[B], eqC: Eq[C], arbA: Arbitrary[A], arbB: Arbitrary[B], arbC: Arbitrary[C], arbOp: Arbitrary[C => C]): RuleSet =
    new SimpleRuleSet("Field3", LensTests(thrd[A, B, C]).lens.props: _*)
}

object Field3Tests {
  def apply[A, B, C]: Field3Tests[A, B, C] = new Field3Tests[A, B, C] {}
}
