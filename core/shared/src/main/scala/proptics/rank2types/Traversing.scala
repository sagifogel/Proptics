package proptics.rank2types

import cats.Applicative

trait Traversing[S, T, A, B] {
  def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T]
}
