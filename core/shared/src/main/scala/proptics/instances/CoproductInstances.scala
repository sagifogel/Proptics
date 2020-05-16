package proptics.instances

import cats.syntax.either._
import cats.data.EitherK
import proptics.Prism_

trait CoproductInstances {
  final def leftK[F[_], G[_], H[_], A]: Prism_[EitherK[F, G, A], EitherK[H, G, A], F[A], H[A]] =
    Prism_ { e: EitherK[F, G, A] => e.run.bimap(identity, ga => EitherK(ga.asRight[H[A]])).swap }(to => EitherK(to.asLeft[G[A]]))

  final def rightK[F[_], G[_], H[_], A]: Prism_[EitherK[F, G, A], EitherK[F, H, A], G[A], H[A]] =
    Prism_ { e: EitherK[F, G, A] => e.run.bimap(fa => EitherK(fa.asLeft[H[A]]), identity) }(to => EitherK(to.asRight[F[A]]))
}
