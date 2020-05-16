package proptics.instances

import cats.data.Tuple2K
import proptics.Lens_

trait ProductInstances {
  final def fst2K[F[_], G[_], H[_], A]: Lens_[Tuple2K[F, G, A], Tuple2K[H, G, A], F[A], H[A]] =
    Lens_((get: Tuple2K[F, G, A]) => get.first)(fgh => ha => Tuple2K[H, G, A](ha, fgh.second))

  final def snd2K[F[_], G[_], H[_], A]: Lens_[Tuple2K[F, G, A], Tuple2K[F, H, A], G[A], H[A]] =
    Lens_((get: Tuple2K[F, G, A]) => get.second)(fgh => ha => Tuple2K(fgh.first, ha))
}
