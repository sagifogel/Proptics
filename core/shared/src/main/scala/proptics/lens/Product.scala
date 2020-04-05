package proptics.lens

import cats.data.Tuple2K
import proptics.Lens_

object Product {
  def _1[F[_], G[_], H[_], A]: Lens_[Tuple2K[F, G, A], Tuple2K[H, G, A], F[A], H[A]] =
    Lens_[Tuple2K[F, G, A], Tuple2K[H, G, A], F[A], H[A]](_.first)(fgh => ha => Tuple2K[H, G, A](ha, fgh.second))

  def fst[F[_], G[_], H[_], A]: Lens_[Tuple2K[F, G, A], Tuple2K[H, G, A], F[A], H[A]] = _1

  def _2[F[_], G[_], H[_], A]: Lens_[Tuple2K[F, G, A], Tuple2K[F, H, A], G[A], H[A]] =
    Lens_[Tuple2K[F, G, A], Tuple2K[F, H, A], G[A], H[A]](_.second)(fgh => ha => Tuple2K(fgh.first, ha))

  def _2[F[_], G[_], H[_], A]: Lens_[Tuple2K[F, G, A], Tuple2K[F, H, A], G[A], H[A]] = _2
}