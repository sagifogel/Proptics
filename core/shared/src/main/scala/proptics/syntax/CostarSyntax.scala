package proptics.syntax

import cats.~>
import proptics.profunctor.Costar

trait CostarSyntax {
  implicit final def costarOps[F[_], A, B](costar: Costar[F, A, B]): CostarOps[F, A, B] = CostarOps(costar)
}

final case class CostarOps[F[_], A, B](private val costar: Costar[F, A, B]) extends AnyVal {
  def runCostar(fa: F[A]): B = costar.run(fa)

  def hoist[G[_]](f: G ~> F): Costar[G, A, B] = Costar.hoistCostar(f)(costar)
}
