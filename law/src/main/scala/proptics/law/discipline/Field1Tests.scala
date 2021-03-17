package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import proptics.instances.field1.{first => fst}
import proptics.typeclass.Field1

trait Field1Tests[A, B] extends Laws {
  def first(implicit ev: Field1[(A, B), A], eqS: Eq[A], eqA: Eq[B], arbA: Arbitrary[A], arbB: Arbitrary[B], arbOp: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet("Field1", LensTests(fst[A, B]).lens.props: _*)
}

object Field1Tests {
  def apply[A, B]: Field1Tests[A, B] = new Field1Tests[A, B] {}
}
