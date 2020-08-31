package proptics.law

import cats.laws._
import cats.syntax.either._
import proptics.profunctor.Cochoice

trait CochoiceLaws[F[_, _]] extends ProfunctorLaws[F] {
  implicit def F: Cochoice[F]

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

  def rightLeftConsistent[A, B, C](fab: F[Either[C, A], Either[C, B]]): IsEq[F[A, B]] =
    F.unright[A, B, C](fab) <->
      F.unleft(F.dimap[Either[C, A], Either[C, B], Either[A, C], Either[B, C]](fab)(_.fold(_.asRight[C], _.asLeft[A]))(_.fold(_.asRight[B], _.asLeft[C])))

  def unleftUnrightConsistent[A, B, C](fab: F[Either[A, C], Either[B, C]]): IsEq[F[A, B]] =
    F.unleft[A, B, C](fab) <->
      F.unright(F.dimap[Either[A, C], Either[B, C], Either[C, A], Either[C, B]](fab)(_.fold(_.asRight[A], _.asLeft[C]))(_.fold(_.asRight[C], _.asLeft[B])))

  def unleftComposeUnleftDimapConsistent[A >: Any, B >: Any, C >: Any](fab: F[Either[Either[A, B], C], Either[Either[B, C], C]]): IsEq[F[A, B]] =
    F.unleft(F.unleft[Either[A, B], Either[B, C], C](fab)) <->
      F.unleft(F.dimap[Either[Either[A, B], C], Either[Either[B, C], C], Either[A, Either[B, C]], Either[B, Either[C, C]]](fab)(unassocE[A, B, C])(assocE[B, C, C]))

  def unrightComposeUnrightDimapConsistent[A >: Any, B >: Any, C >: Any](fab: F[Either[C, Either[A, B]], Either[C, Either[B, C]]]): IsEq[F[B, C]] =
    F.unright(F.unright[Either[A, B], Either[B, C], C](fab)) <->
      F.unright(F.dimap[Either[C, Either[A, B]], Either[C, Either[B, C]], Either[Either[C, A], B], Either[Either[C, B], C]](fab)(assocE)(unassocE))
}

object CochoiceLaws {
  def apply[F[_, _]](implicit ev: Cochoice[F]): CochoiceLaws[F] =
    new CochoiceLaws[F] { def F: Cochoice[F] = ev }
}
