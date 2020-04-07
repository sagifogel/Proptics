package proptics.prism

import cats.syntax.either._
import cats.data.EitherK
import proptics.Prism_

object Coproduct {
  def left[F[_], G[_], H[_], A]: Prism_[EitherK[F, G, A], EitherK[H, G, A], F[A], H[A]] =
    Prism_((to: H[A]) =>
      EitherK(to.asLeft[G[A]]))(_.run.bimap(identity, ga => EitherK(ga.asRight[H[A]])).swap)

  def right[F[_], G[_], H[_], A]: Prism_[EitherK[F, G, A], EitherK[F, H, A], G[A], H[A]] =
    Prism_((to: H[A]) =>
      EitherK(to.asRight[F[A]]))(_.run.bimap(fa => EitherK(fa.asLeft[H[A]]), identity))
}
