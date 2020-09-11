package proptics.law

import proptics.internal.Wander

trait WanderLaws[F[_, _]] {
  implicit def F: Wander[F]
}

object WanderLaws {
  def apply[F[_, _]](implicit ev: Wander[F]): WanderLaws[F] =
    new WanderLaws[F] { def F: Wander[F] = ev }
}
