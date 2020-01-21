package proptics.syntax

import proptics.Optic
import proptics.profunctor.Costar

object CostarSyntax {
  implicit class CostarOps[F[_], S, T, A, B](val costar: Optic[Costar[F, *, *], S, T, A, B]) extends AnyVal {
    def zipFWithOf(f: F[A] => B): F[S] => T = costar(Costar(f)).runCostar
  }
}
