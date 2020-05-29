package proptics.profunctor

import cats.Applicative
import cats.arrow.Profunctor
import cats.syntax.either._

/**
  * The Choice type class extends [[Profunctor]] with combinators for working with sum types.
  * @see [[Choice#left]] and [[Choice#right]] lift values in a [[Profunctor]] to act on the [[Left]] and
  * [[Right]] components of a sum, respectively.
  *
  * @tparam P type constructor of kind (* -> * -> *)
  */
trait Choice[P[_, _]] extends Profunctor[P] {
  def left[A, B, C](pab: P[A, B]): P[Either[A, C], Either[B, C]]

  def right[A, B, C](pab: P[B, C]): P[Either[A, B], Either[A, C]]
}

abstract class ChoiceInstances {
  implicit final val choiceFunction: Choice[* => *] = new Choice[* => *] {
    override def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C] = _.leftMap(pab)

    override def right[A, B, C](pab: B => C): Either[A, B] => Either[A, C] = _.map(pab)

    override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = g compose fab compose f
  }

  implicit final def choiceStar[F[_]](implicit ev: Applicative[F]): Choice[Star[F, *, *]] = new Choice[Star[F, *, *]] {
    override def left[A, B, C](pab: Star[F, A, B]): Star[F, Either[A, C], Either[B, C]] =
      Star {
        case Left(a)  => ev.map(pab.runStar(a))(_.asLeft[C])
        case Right(c) => ev.pure(c.asRight[B])
      }

    override def right[A, B, C](pab: Star[F, B, C]): Star[F, Either[A, B], Either[A, C]] =
      Star {
        case Left(a)  => ev.pure(a.asLeft[C])
        case Right(b) => ev.map(pab.runStar(b))(_.asRight[A])
      }

    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] =
      Star(ev.lift(g) compose fab.runStar compose f)
  }
}

object Choice extends ChoiceInstances
