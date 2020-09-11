package proptics.law.discipline

import cats.Eq
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import proptics.instances.At
import proptics.instances.at.{at => atFn}

trait AtTests[S, I, A] extends Laws {
  def at(implicit
      ev: At[S, I, A],
      eqS: Eq[S],
      eqA: Eq[A],
      arbS: Arbitrary[S],
      arbI: Arbitrary[I],
      arbA: Arbitrary[A],
      arbAA: Arbitrary[A => A],
      arbOp: Arbitrary[Option[A] => Option[A]]): RuleSet =
    new SimpleRuleSet("At", LensTests(atFn(_: I)(ev)).lens.props: _*)
}

object AtTests {
  def apply[S, I, A]: AtTests[S, I, A] =
    new AtTests[S, I, A] {}
}
