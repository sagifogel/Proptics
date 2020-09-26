package proptics

import cats.data.Kleisli

package object profunctor {
  type Star[F[_], -A, B] = Kleisli[F, A, B]
}
