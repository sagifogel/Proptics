package proptics.profunctor

import scala.annotation.implicitNotFound

import cats.Applicative
import cats.arrow.Profunctor
import cats.syntax.either._

/** The Choice type class extends [[Profunctor]] with combinators for working with sum types.
  * @see [[Choice#left]] and [[Choice#right]] lift values in a [[Profunctor]] to act on the [[Left]] and
  * [[Right]] components of a sum, respectively.
  *
  * @tparam P type constructor of kind (* -> * -> *)
  */
@implicitNotFound("Could not find an instance of Choice[${P}]")
trait Choice[P[_, _]] extends Profunctor[P] {
  def left[A, B, C](pab: P[A, B]): P[Either[A, C], Either[B, C]]

  def right[A, B, C](pab: P[A, B]): P[Either[C, A], Either[C, B]]
}

abstract class ChoiceInstances {
  implicit final val choiceFunction: Choice[* => *] = new Choice[* => *] {
    override def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C] = _.leftMap(pab)

    override def right[A, B, C](pab: A => B): Either[C, A] => Either[C, B] = _.map(pab)

    override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = g compose fab compose f
  }

  implicit final def choiceStar[F[_]](implicit ev: Applicative[F]): Choice[Star[F, *, *]] = new Choice[Star[F, *, *]] {
    override def left[A, B, C](pab: Star[F, A, B]): Star[F, Either[A, C], Either[B, C]] =
      Star {
        case Left(a) => ev.map(pab.run(a))(_.asLeft[C])
        case Right(c) => ev.pure(c.asRight[B])
      }

    override def right[A, B, C](pab: Star[F, A, B]): Star[F, Either[C, A], Either[C, B]] =
      Star {
        case Left(c) => ev.pure(c.asLeft[B])
        case Right(a) => ev.map(pab.run(a))(_.asRight[C])
      }

    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] =
      Star(ev.lift(g) compose fab.run compose f)

  }
}

object Choice extends ChoiceInstances {
  /** summon an instance of [[Choice]] for `P` */
  @inline def apply[P[_, _]](implicit ev: Choice[P]): Choice[P] = ev
}
