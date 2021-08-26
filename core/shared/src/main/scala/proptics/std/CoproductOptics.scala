package proptics.std

import cats.data.EitherK
import cats.syntax.either._

import proptics.Prism_

trait CoproductOptics {
  final def leftK[F[_], G[_], H[_], A]: Prism_[EitherK[F, G, A], EitherK[H, G, A], F[A], H[A]] = {
    Prism_((e: EitherK[F, G, A]) => e.run.map(ga => EitherK(ga.asRight[H[A]])).swap)(to => EitherK(to.asLeft[G[A]]))
  }

  final def rightK[F[_], G[_], H[_], A]: Prism_[EitherK[F, G, A], EitherK[F, H, A], G[A], H[A]] =
    Prism_((e: EitherK[F, G, A]) => e.run.leftMap(fa => EitherK(fa.asLeft[H[A]])))(to => EitherK(to.asRight[F[A]]))
}
