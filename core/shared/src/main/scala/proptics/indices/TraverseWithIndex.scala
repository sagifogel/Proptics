package proptics.indices

import scala.annotation.implicitNotFound

import cats.Applicative

@implicitNotFound("Could not find an instance of TraversalWithIndex[${F}, ${I}]")
trait TraverseWithIndex[F[_], I] extends FoldableWithIndex[F, I] with FunctorWithIndex[F, I] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  def traverseWithIndex[G[_]: Applicative, A, B](f: (A, I) => G[B])(fa: F[A]): G[F[B]] =
    sequence(mapWithIndex(f)(fa))
}

object TraverseWithIndex {
  /** summon an instance of [[proptics.indices.TraverseWithIndex]] for `F` */
  @inline def apply[F[_], I](implicit instance: TraverseWithIndex[F, I]): TraverseWithIndex[F, I] = instance
}
