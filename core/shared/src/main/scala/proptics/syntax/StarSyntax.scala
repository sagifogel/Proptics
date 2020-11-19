package proptics.syntax

import cats.~>

import proptics.profunctor.Star

trait StarSyntax {
  implicit final def starOps[F[_], A, B](star: Star[F, A, B]): StarOps[F, A, B] = StarOps(star)
}

final case class StarOps[F[_], -A, B](private val star: Star[F, A, B]) extends AnyVal {
  /** synonym to Kleisli run */
  def runStar(a: A): F[B] = star.run(a)

  /** apply a natural transformation from a Functor of F to a Functor of G */
  def hoist[G[_]](f: F ~> G): Star[G, A, B] = Star.hoist(f)(star)
}
