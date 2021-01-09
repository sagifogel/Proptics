package proptics.syntax

import cats.Applicative

import proptics.indices.TraverseWithIndex

trait TraverseWithIndexSyntax {
  implicit final def traverseWithIndexOps[F[_], A](fa: F[A]): TraverseWithIndexOps[F, A] = TraverseWithIndexOps[F, A](fa)
}

final case class TraverseWithIndexOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def traverseWithIndex[G[_]: Applicative, I, B](f: (A, I) => G[B])(implicit ev: TraverseWithIndex[F, I]): G[F[B]] =
    ev.traverseWithIndex(f)(fa)

}
