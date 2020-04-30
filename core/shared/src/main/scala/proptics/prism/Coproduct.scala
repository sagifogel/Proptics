package proptics.prism

import cats.syntax.either._
import cats.data.EitherK
import proptics.Prism_

object Coproduct {
  def left[F[_], G[_], H[_], A]: Prism_[EitherK[F, G, A], EitherK[H, G, A], F[A], H[A]] =
    Prism_ { e: EitherK[F, G, A] => e.run.bimap(identity, ga => EitherK(ga.asRight[H[A]])).swap }(to => EitherK(to.asLeft[G[A]]))

  def right[F[_], G[_], H[_], A]: Prism_[EitherK[F, G, A], EitherK[F, H, A], G[A], H[A]] =
    Prism_ { e: EitherK[F, G, A] => e.run.bimap(fa => EitherK(fa.asLeft[H[A]]), identity) }(to => EitherK(to.asRight[F[A]]))
}
