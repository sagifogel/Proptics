package proptics

import cats.Eq
import cats.instances.tuple._
import cats.syntax.bifunctor._
import cats.arrow.Strong
import cats.data.State
import org.scalacheck.Arbitrary
import proptics.newtype.Disj
import proptics.profunctor.Star
import Function.const

package object specs {
  val whole9: Whole = Whole(9)
  val greaterThan5: Int => Boolean = _ > 5
  val greaterThan10: Int => Boolean = _ > 10
  implicit val state: State[Whole, Int] = State.pure[Whole, Int](9)
  implicit val eqWhole: Eq[Whole] = Eq.fromUniversalEquals[Whole]
  implicit val arbitraryWhole: Arbitrary[Whole] = Arbitrary[Whole](Arbitrary.arbInt.arbitrary.map(Whole))

  implicit def strongStarTupleOfDisj: Strong[Star[(Disj[Boolean], *), *, *]] = new Strong[Star[(Disj[Boolean], *), *, *]] {
    override def first[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (A, C), (B, C)] =
      Star {
        case (a, c) =>
          fa.runStar(a).bimap(identity, (_, c))
      }

    override def second[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (C, A), (C, B)] =
      Star {
        case (c, a) =>
          fa.runStar(a).bimap(identity, (c, _))
      }

    override def dimap[A, B, C, D](fab: Star[(Disj[Boolean], *), A, B])(f: C => A)(g: B => D): Star[(Disj[Boolean], *), C, D] =
      Star(c => fab.runStar(f(c)).bimap(identity, g))
  }

  implicit def strongStarTupleOfNegativeDisj: Strong[Star[(Disj[Boolean], *), *, *]] = new Strong[Star[(Disj[Boolean], *), *, *]] {
    override def first[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (A, C), (B, C)] =
      Star {
        case (a, c) =>
          fa.runStar(a).bimap(const(Disj(false)), (_, c))
      }

    override def second[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (C, A), (C, B)] =
      Star {
        case (c, a) =>
          fa.runStar(a).bimap(const(Disj(false)), (c, _))
      }

    override def dimap[A, B, C, D](fab: Star[(Disj[Boolean], *), A, B])(f: C => A)(g: B => D): Star[(Disj[Boolean], *), C, D] =
      Star(c => fab.runStar(f(c)).bimap(const(Disj(false)), g))
  }
}
