package proptics.syntax

import proptics.Optic
import proptics.profunctor.Star

object StarSyntax {
  implicit class StarOps[F[_], S, T, A, B](val star: Optic[Star[F, *, *], S, T, A, B]) extends AnyVal {
    def collectOf(f: A => F[B]): S => F[T] = star(Star(f)).runStar
  }
}
