package proptics

import cats.data.Kleisli

package object profunctor {
  /**
    * [[Star]] turns a Functor into a Profunctor "forwards".
    * <p>
    * [[Star]] is a type alias of Kleisli.
    * </p>
    */
  type Star[F[_], -A, B] = Kleisli[F, A, B]
}
