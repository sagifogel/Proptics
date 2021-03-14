package proptics.std

import scala.{Function => F}

import proptics.Iso_.iso
import proptics.syntax.function._
import proptics.{Iso, Iso_}

trait FunctionOptics {
  /** a polymorphic [[Iso_]] for currying and uncurrying a function */
  final def curriedP[A, B, C, D, E, F]: Iso_[(A, B) => C, (D, E) => F, A => B => C, D => E => F] =
    iso[(A, B) => C, (D, E) => F, A => B => C, D => E => F](_.curried)(F.uncurried[D, E, F])

  /** a polymorphic [[Iso_]] for uncurrying and currying a function */
  final def uncurriedP[A, B, C, D, E, F]: Iso_[A => B => C, D => E => F, (A, B) => C, (D, E) => F] =
    iso[A => B => C, D => E => F, (A, B) => C, (D, E) => F](F.uncurried[A, B, C])(_.curried)

  /** a polymorphic [[Iso_]] for flipping a function */
  final def flipP[A, B, C, D, E, F]: Iso_[A => B => C, D => E => F, B => A => C, E => D => F] =
    iso[A => B => C, D => E => F, B => A => C, E => D => F](_.flip)(_.flip)

  /** a monomorphic [[Iso]] for currying and uncurrying a function */
  final def curried[A, B, C]: Iso[(A, B) => C, A => B => C] = curriedP[A, B, C, A, B, C]

  /** a monomorphic [[Iso]] for uncurrying and currying a function */
  final def uncurried[A, B, C]: Iso[A => B => C, (A, B) => C] = uncurriedP[A, B, C, A, B, C]

  /** a monomorphic [[Iso]] for flipping a function */
  final def flip[A, B, C]: Iso[A => B => C, B => A => C] = flipP[A, B, C, A, B, C]
}
