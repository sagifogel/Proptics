package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.field2.{second => snd}
import proptics.typeclass.Field2

trait Field2Tests[A, B] extends Laws {
  def second(implicit ev: Field2[(A, B), B], eqA: Eq[A], eqB: Eq[B], arbA: Arbitrary[A], arbB: Arbitrary[B], arbOp: Arbitrary[B => B]): RuleSet =
    new SimpleRuleSet("field2", LensTests(snd[A, B]).lens.props: _*)
}

object Field2Tests {
  def apply[A, B]: Field2Tests[A, B] = new Field2Tests[A, B] {}
}
