package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.field5.{fifth => ffth}
import proptics.typeclass.Field5

trait Field5Tests[A, B, C, D, E] extends Laws {
  def fifth(
      implicit ev: Field5[(A, B, C, D, E), E],
      eqA: Eq[A],
      eqB: Eq[B],
      eqC: Eq[C],
      eqD: Eq[D],
      eqE: Eq[E],
      arbA: Arbitrary[A],
      arbB: Arbitrary[B],
      arbC: Arbitrary[C],
      arbD: Arbitrary[D],
      arbE: Arbitrary[E],
      arbOp: Arbitrary[E => E]): RuleSet =
    new SimpleRuleSet("field5", LensTests(ffth[A, B, C, D, E]).lens.props: _*)
}

object Field5Tests {
  def apply[A, B, C, D, E]: Field5Tests[A, B, C, D, E] = new Field5Tests[A, B, C, D, E] {}
}
