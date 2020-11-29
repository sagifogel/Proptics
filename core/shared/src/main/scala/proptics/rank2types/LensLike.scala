package proptics.rank2types

import cats.Applicative

trait LensLike[S, T, A, B] {
  def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T]
}
