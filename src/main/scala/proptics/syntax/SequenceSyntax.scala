package proptics.syntax

import cats.arrow.Strong
import proptics.Lens
import proptics.profunctor.Star

object SequenceSyntax {
  implicit class SequenceLensStarOps[F[_], S, T, A](val lens: Lens[S, T, F[A], A]) extends AnyVal {
    def sequence(implicit ev: Strong[Star[F, *, *]]): S => F[T] = lens.traverse(identity)
  }
}
