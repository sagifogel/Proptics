package proptics.syntax

import cats.Applicative

import proptics.ATraversal_

trait ATraversalSyntax {
  implicit def aTraversalSequenceOps[F[_], I, S, T, A](aTraversal: ATraversal_[S, T, F[A], A]): ATraversalSequenceOps[F, I, S, T, A] = ATraversalSequenceOps(aTraversal)
}

final case class ATraversalSequenceOps[F[_], I, S, T, A](private val grate: ATraversal_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = grate.traverse(s)(identity)
}
