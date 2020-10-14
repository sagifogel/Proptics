package proptics

import cats.data.{Cokleisli, Kleisli}

package object profunctor {

  /** [[Star]] turns a Functor into a Profunctor "forwards".
    * <p>
    * [[Star]] is a type alias of Kleisli.
    * </p>
    */
  type Star[F[_], -A, B] = Kleisli[F, A, B]

  /** [[Costar]] turns a Functor into a Profunctor "backwards".
    * <p>
    * [[Costar]] is a type alias of Cokleisli.
    * </p>
    */
  type Costar[F[_], A, B] = Cokleisli[F, A, B]
}
