package proptics.syntax

import cats.Functor
import proptics.Lens

object SequenceSyntax {
  implicit class SequenceLensStarOps[F[_], S, T, A](val lens: Lens[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Functor[F]): F[T] = lens.traverse(s)(identity)
  }
}
