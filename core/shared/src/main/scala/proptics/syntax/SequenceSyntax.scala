package proptics.syntax

import cats.Applicative
import proptics._

object SequenceSyntax {
  implicit class IndexedTraversalSequenceOps[F[_], I, S, T, A](val iso: IndexedTraversal_[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(_._2)
  }
}
