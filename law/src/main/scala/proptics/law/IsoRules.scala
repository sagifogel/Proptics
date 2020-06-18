package proptics.law

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import proptics.Iso

object IsoRules extends Laws {
  def apply[S: Arbitrary: Eq, A: Arbitrary: Eq](iso: Iso[S, A]): RuleSet = {
    val laws = IsoLaws(iso)

    new SimpleRuleSet(
      "Iso",
      "reversibility" -> forAll(laws.reversibility _)
    )
  }
}
