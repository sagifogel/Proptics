package proptics.syntax

import proptics.Optic
import proptics.profunctor.Star

object TraverseSyntax {
  implicit class TraverseOfStatOps[P[_, _], F[_], S, T, A, B](val traverse: Optic[Star[F, *, *], S, T, A, B]) extends AnyVal {
    def traverseOf(f: A => F[B])(s: S): F[T] = traverse(Star(f)).runStar(s)
  }

  implicit class SequenceOfOps[P[_, _], F[_], S, T, A, B](val traverse: Optic[Star[F, *, *], S, T, F[A], A]) extends AnyVal {
    def sequenceOf(s: S): F[T] = traverse.traverseOf(identity)(s)
  }
}
