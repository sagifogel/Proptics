package proptics.rank2types

import cats.Applicative

private[proptics] trait LensLikeWithIndex[I, S, T, A, B] {
  def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): S => F[T]
}
