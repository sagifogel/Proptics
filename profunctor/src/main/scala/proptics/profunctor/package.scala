package proptics

import cats.data.{Cokleisli, Kleisli}

package object profunctor {

  /** A [[Star]] turns a [[cats.Functor]] into a [[cats.arrow.Profunctor]] "forwards".
    *
    * A [[Star]] is a type alias of [[cats.data.Kleisli]].
    */
  type Star[F[_], -A, B] = Kleisli[F, A, B]

  /** [[Costar]] turns a [[cats.Functor]] into a [[cats.arrow.Profunctor]] "backwards".
    *
    * [[Costar]] is a type alias of [[cats.data.Cokleisli]]
    */
  type Costar[F[_], A, B] = Cokleisli[F, A, B]
}
