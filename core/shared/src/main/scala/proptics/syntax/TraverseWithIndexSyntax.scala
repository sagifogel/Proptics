package proptics.syntax

import cats.Applicative

import proptics.indices.TraverseWithIndex

trait TraverseWithIndexSyntax {
  implicit final def traverseWithIndexOps[F[_], A](fa: F[A]): TraverseWithIndexOps[F, A] = TraverseWithIndexOps[F, A](fa)
}

final case class TraverseWithIndexOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def traverse[G[_]: Applicative, B](f: A => G[B])(implicit ev0: TraverseWithIndex[F, _]): G[F[B]] =
    ev0.traverse(fa)(f)

  def traverseWithIndex[G[_]: Applicative, I, B](f: (A, I) => G[B])(implicit ev0: TraverseWithIndex[F, I]): G[F[B]] =
    ev0.traverseWithIndex(f)(fa)
}
