package proptics.law.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen}
import proptics.law.ClosedLaws
import proptics.profunctor.Closed

trait ClosedTests[F[_, _]] extends ProfunctorTests[F] {
  def laws: ClosedLaws[F]

  def closed[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary, E: Arbitrary, G: Arbitrary](implicit
      ArbFAB: Arbitrary[F[A, B]],
      ArbFAA: Arbitrary[F[A, A]],
      ArbFCD: Arbitrary[F[C, D]],
      CogenA: Cogen[A],
      CogenB: Cogen[B],
      CogenC: Cogen[C],
      CogenD: Cogen[D],
      CogenE: Cogen[E],
      CogenCACA: Cogen[(C => A) => C => A],
      CogenCBCB: Cogen[(C => B) => C => B],
      EqFAB: Eq[F[A, B]],
      EqFAD: Eq[F[A, D]],
      EqFAG: Eq[F[A, G]],
      EqAAAA: Eq[F[A => A, A => A]],
      EqCACB: Eq[F[C => A, C => B]],
      EqAAAAAB: Eq[F[A => A => A, A => A => B]]): RuleSet =
    new DefaultRuleSet(
      name = "closed",
      parent = Some(profunctor[A, B, C, D, E, G]),
      "lmap closed rmap consistent" -> forAll(laws.lmapClosedRmapClosedConsistent[A] _),
      "closed compose closed dimap consistent" -> forAll(laws.closedComposeClosedDimapConsistent[A, B] _),
      "dimap const identity consistent" -> forAll(laws.dimapConstIdentityConsistent[A, B] _)
    )
}

object ClosedTests {
  def apply[F[_, _]: Closed]: ClosedTests[F] =
    new ClosedTests[F] { def laws: ClosedLaws[F] = ClosedLaws[F] }
}
