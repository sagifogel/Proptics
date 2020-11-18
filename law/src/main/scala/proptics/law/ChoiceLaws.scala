package proptics.law

import cats.laws._
import cats.syntax.either._
import cats.syntax.profunctor._

import proptics.profunctor.Choice

trait ChoiceLaws[F[_, _]] extends ProfunctorLaws[F] {
  implicit def F: Choice[F]

  private def assocE[A, B, C](either: Either[Either[A, B], C]): Either[A, Either[B, C]] = either match {
    case Left(Left(a))  => Left(a)
    case Left(Right(b)) => Right(Left(b))
    case Right(c)       => Right(Right(c))
  }

  private def unassocE[A, B, C](either: Either[A, Either[B, C]]): Either[Either[A, B], C] = either match {
    case Left(a)         => Left(Left(a))
    case Right(Left(b))  => Left(Right(b))
    case Right(Right(c)) => Right(c)
  }

  def leftRightConsistent[A, B, C](fab: F[A, B]): IsEq[F[Either[A, C], Either[B, C]]] =
    F.left[A, B, C](fab) <->
      F.right[A, B, C](fab).dimap((x: Either[A, C]) => x.swap)((y: Either[C, B]) => y.swap)

  def rightLeftConsistent[A, B, C](fab: F[A, B]): IsEq[F[Either[C, A], Either[C, B]]] =
    F.right[A, B, C](fab) <->
      F.left[A, B, C](fab).dimap((x: Either[C, A]) => x.swap)((y: Either[B, C]) => y.swap)

  def leftRmapLmapConsistent[A, B, C](fab: F[A, B]): IsEq[F[A, Either[B, C]]] =
    F.rmap[A, B, Either[B, C]](fab)(_.asLeft[C]) <-> F.left[A, B, C](fab).lmap(_.asLeft[C])

  def rightLmapRmapConsistent[A, B, C](fab: F[A, B]): IsEq[F[A, Either[C, B]]] =
    F.rmap[A, B, Either[C, B]](fab)(_.asRight[C]) <-> F.right[A, B, C](fab).lmap(_.asRight[C])

  def leftComposeLeftDimapConsistent[A, B, C](fab: F[A, B]): IsEq[F[Either[Either[A, C], B], Either[Either[B, C], B]]] =
    F.left[Either[A, C], Either[B, C], B](F.left[A, B, C](fab)) <->
      F.left[A, B, Either[C, B]](fab).dimap(assocE[A, C, B])(unassocE[B, C, B])

  def rightComposeRightDimapConsistent[A, B, C](fab: F[A, B]): IsEq[F[Either[B, Either[C, A]], Either[B, Either[C, B]]]] =
    F.right[Either[C, A], Either[C, B], B](F.right[A, B, C](fab)) <->
      F.right[A, B, Either[B, C]](fab).dimap(unassocE[B, C, A])(assocE)
}

object ChoiceLaws {
  def apply[F[_, _]](implicit ev: Choice[F]): ChoiceLaws[F] =
    new ChoiceLaws[F] { def F: Choice[F] = ev }
}
