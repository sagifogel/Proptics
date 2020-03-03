package proptics.syntax

import cats.arrow.Strong
import proptics.Lens
import proptics.profunctor.Star

object SequenceOfSyntax {
  implicit class SequenceOfLensStarOps[F[_], S, T, A](val lens: Lens[S, T, F[A], A]) extends AnyVal {
    def sequenceOf(s: S)(implicit ev: Strong[Star[F, *, *]]): F[T] = lens.traverseOf(identity)(s)
  }
}
