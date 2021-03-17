package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.field4.{fourth => frth}
import proptics.typeclass.Field4

trait Field4Tests[A, B, C, D] extends Laws {
  def fourth(
      implicit ev: Field4[(A, B, C, D), D],
      eqA: Eq[A],
      eqB: Eq[B],
      eqC: Eq[C],
      eqD: Eq[D],
      arbA: Arbitrary[A],
      arbB: Arbitrary[B],
      arbC: Arbitrary[C],
      arbD: Arbitrary[D],
      arbOp: Arbitrary[D => D]): RuleSet =
    new SimpleRuleSet("Field4", LensTests(frth[A, B, C, D]).lens.props: _*)
}

object Field4Tests {
  def apply[A, B, C, D]: Field4Tests[A, B, C, D] = new Field4Tests[A, B, C, D] {}
}
