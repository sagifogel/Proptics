package proptics.syntax

import cats.syntax.eq._
import cats.{Applicative, Eq}

import proptics.IndexedTraversal_
import proptics.IndexedTraversal_.wander
import proptics.internal.Indexed
import proptics.profunctor.Star
import proptics.rank2types.Rank2TypeLensLikeWithIndex
import proptics.syntax.star._

trait IndexedTraversalSyntax {
  implicit def indexedTraversalElementsOps[I, S, T, A](indexedTraversal: IndexedTraversal_[I, S, T, A, A]): IndexedTraversalElementsOps[I, S, T, A] =
    IndexedTraversalElementsOps(indexedTraversal)

  implicit def indexedTraversalSequenceOps[F[_], I, S, T, A](indexedTraversal: IndexedTraversal_[I, S, T, F[A], A]): IndexedTraversalSequenceOps[F, I, S, T, A] =
    IndexedTraversalSequenceOps(indexedTraversal)
}

final case class IndexedTraversalSequenceOps[F[_], I, S, T, A](private val iso: IndexedTraversal_[I, S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(_._2)
}

final case class IndexedTraversalElementsOps[I, S, T, A](private val indexedTraversal: IndexedTraversal_[I, S, T, A, A]) extends AnyVal {
  def element(i: I)(implicit ev: Eq[I]): IndexedTraversal_[I, S, T, A, A] = filterByIndex(_ === i)

  def filterByIndex(pr: I => Boolean): IndexedTraversal_[I, S, T, A, A] =
    wander(new Rank2TypeLensLikeWithIndex[I, S, T, A, A] {
      override def apply[F[_]](f: ((I, A)) => F[A])(implicit ev: Applicative[F]): S => F[T] = {
        val starIndex: Indexed[Star[F, *, *], I, A, A] = Indexed[Star[F, *, *], I, A, A](Star { case (i, a) =>
          if (pr(i)) f((i, a)) else ev.pure(a)
        })

        indexedTraversal(starIndex).runStar
      }
    })
}
