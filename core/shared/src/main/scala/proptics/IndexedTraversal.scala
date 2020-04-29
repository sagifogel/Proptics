package proptics

import cats.data.Const
import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, Order, Traverse}
import proptics.IndexedLens_.liftIndexedOptic
import proptics.instances.BooleanInstances._
import proptics.internal._
import proptics.newtype._
import proptics.profunctor.Star
import proptics.rank2types.{Rank2TypeIndexedTraversalLike, Rank2TypeLensLikeWithIndex, Rank2TypeTraversalLike}
import proptics.syntax.function._
import proptics.syntax.tuple._
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
  * An [[IndexedTraversal_]] is An indexed optic constrained with [[Wander]] [[cats.arrow.Profunctor]]
  *
  * @tparam I the index of an [[IndexedTraversal_]]
  * @tparam S the source of an [[IndexedTraversal_]]
  * @tparam T the modified source of an [[IndexedTraversal_]]
  * @tparam A the target of an [[IndexedTraversal_]]
  * @tparam B the modified target of an [[IndexedTraversal_]]
  */
abstract class IndexedTraversal_[I, S, T, A, B] extends Serializable { self =>
  def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T]

  def view(s: S)(implicit ev: Monoid[(I, A)]): (I, A) = foldMap(s)(identity)

  def viewAll(s: S)(implicit ev: Monoid[(I, A)]): List[(I, A)] = foldMap(s)(List(_))

  def preview(s: S): Option[(I, A)] = foldMapNewtype[First[(I, A)], Option[(I, A)]](s)(_.some)

  def set(b: B): S => T = over(const(b))

  def over(f: ((I, A)) => B): S => T = self(Indexed(f))

  def overF[F[_]: Applicative](f: ((I, A)) => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]: Applicative](s: S)(f: ((I, A)) => F[B]): F[T] =
    self[Star[F, *, *]](Indexed(Star[F, (I, A), B](f))).runStar(s)

  def foldMap[R: Monoid](s: S)(f: ((I, A)) => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  def fold(s: S)(implicit ev: Monoid[(I, A)]): (I, A) = foldMap(s)(identity)

  def foldr[R](s: S)(r: R)(f: ((I, A)) => R => R): R = foldMap(s)(Endo[* => *, R] _ compose f).runEndo(r)

  def foldl[R](s: S)(r: R)(f: R => ((I, A)) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip).runDual.runEndo(r)

  def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  def traverse_[F[_], R](s: S)(f: ((I, A)) => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldr[F[Unit]](s)(ev.pure(()))(ia => ev.void(f(ia)) *> _)

  def sum(s: S)(implicit ev: Semiring[(I, A)]): (I, A) = foldMapNewtype[Additive[(I, A)], (I, A)](s)(identity)

  def product(s: S)(implicit ev: Semiring[(I, A)]): (I, A) = foldMapNewtype[Multiplicative[(I, A)], (I, A)](s)(identity)

  def all(f: ((I, A)) => Boolean): S => Boolean = s => allOf(s)(f)

  def allOf[R: Heyting](s: S)(f: ((I, A)) => R): R = foldMapNewtype[Conj[R], R](s)(f)

  def and(s: S)(implicit ev: Heyting[(I, A)]): (I, A) = allOf(s)(identity)

  def or(s: S)(implicit ev: Heyting[(I, A)]): (I, A) = anyOf[Id, (I, A)](s)(identity)

  def exists(f: ((I, A)) => Boolean): S => Boolean = s => anyOf[Disj, Boolean](s)(f)

  def anyOf[F[_], R: Heyting](s: S)(f: ((I, A)) => R): R = foldMapNewtype[Disj[R], R](s)(f)

  def contains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = !contains(s)(a)

  def length(s: S): Int = foldMap(s)(const(1))

  def has[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

  def hasNot[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)

  def find(f: ((I, A)) => Boolean): S => Option[(I, A)] = s => foldr[Option[(I, A)]](s)(None)(ia => _.fold(if (f(ia)) ia.some else None)(Some[(I, A)]))

  def first(s: S): Option[(I, A)] = preview(s)

  def last(s: S): Option[(I, A)] = foldMapNewtype[Last[(I, A)], Option[(I, A)]](s)(_.some)

  def minimum(s: S)(implicit ev: Order[(I, A)]): Option[(I, A)] = minMax(s)(ev.min)

  def maximum(s: S)(implicit ev: Order[(I, A)]): Option[(I, A)] = minMax(s)(ev.max)

  def toArray[AA >: (I, A)](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[(I, A)]): Array[AA] = toList(s).toArray

  def toList(s: S)(implicit ev: Monoid[(I, A)]): List[(I, A)] = viewAll(s)

  def use[M[_]](implicit ev0: MonadState[M, S], ev1: Monoid[(I, A)]): M[List[(I, A)]] = ev0.inspect(viewAll)

  def zipWith[F[_]](f: ((I, A)) => ((I, A)) => B): S => S => T = self(Indexed(Zipping(f))).runZipping

  def unIndex: Traversal_[S, T, A, B] =
    Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] =
        self(Indexed(ev.dimap[A, B, (I, A), B](pab)(_._2)(identity)))
    })

  def asTraversal: Traversal_[S, T, A, B] = unIndex

  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (I, C), D] = new Traversing[S, T, (I, C), D] {
        override def apply[F[_]](f: ((I, C)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (_, a) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = self compose other.asIndexedLens

  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (I, C), D] = new Traversing[S, T, (I, C), D] {
        override def apply[F[_]](f: ((I, C)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (_, a) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  def compose[C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B](other(indexed) compose Tuple2._2))
  }

  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = self compose other.asIndexedFold

  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] = {
      val runForget = other(indexed).runForget

      Forget(self.foldMap(_)(runForget compose Tuple2._2))
    }
  }

  private def hasOrHasnt[R: Heyting](s: S)(r: R): R = foldMap(s)(const(Disj(r))).runDisj

  private def foldMapNewtype[F: Monoid, R](s: S)(f: ((I, A)) => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: ((I, A), (I, A)) => (I, A))(implicit ev: Order[(I, A)]): Option[(I, A)] =
    foldr[Option[(I, A)]](s)(None)(a => op => f(a, op.getOrElse(a)).some)
}

object IndexedTraversal_ {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedTraversalLike[I, S, T, A, B]): IndexedTraversal_[I, S, T, A, B] = new IndexedTraversal_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = f(indexed)
  }

  def apply[I, S, T, A, B](get: S => (I, A))(_set: S => B => T): IndexedTraversal_[I, S, T, A, B] = new IndexedTraversal_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (I, A), B] = new Traversing[S, T, (I, A), B] {
        override def apply[F[_]](f: ((I, A)) => F[B])(s: S)(implicit ev: Applicative[F]): F[T] =
          ev.map(f(get(s)))(_set(s))
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  def apply[I, S, T, A, B](to: S => ((I, A), B => T)): IndexedTraversal_[I, S, T, A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(indexed.runIndex)
    })

  def fromTraverse[G[_], I, A, B](implicit ev0: Traverse[G]): IndexedTraversal_[I, G[(I, A)], G[B], A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, G[(I, A)], G[B], A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev1: Wander[P]): P[G[(I, A)], G[B]] = {
        val traversing = new Traversing[G[(I, A)], G[B], (I, A), B] {
          override def apply[F[_]](f: ((I, A)) => F[B])(s: G[(I, A)])(implicit ev2: Applicative[F]): F[G[B]] =
            ev0.traverse[F, (I, A), B](s)(f)
        }

        ev1.wander(traversing)(indexed.runIndex)
      }
    })

  def wander[I, S, T, A, B](itr: Rank2TypeLensLikeWithIndex[I, S, T, A, B]): IndexedTraversal_[I, S, T, A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev0: Wander[P]): P[S, T] = {
        def traversing: Traversing[S, T, (I, A), B] = new Traversing[S, T, (I, A), B] {
          override def apply[F[_]](f: ((I, A)) => F[B])(s: S)(implicit ev1: Applicative[F]): F[T] = itr[F](f)(ev1)(s)
        }

        ev0.wander(traversing)(indexed.runIndex)
      }
    })
}

object IndexedTraversal {
  def apply[I, S, A](get: S => (I, A))(set: S => A => S): IndexedTraversal[I, S, A] = IndexedTraversal_(get)(set)

  def apply[I, S, A](to: S => ((I, A), A => S)): IndexedTraversal[I, S, A] = traversal(to)

  def traversal[I, S, A](to: S => ((I, A), A => S)): IndexedTraversal[I, S, A] = IndexedTraversal_[I, S, S, A, A](to)

  def fromTraverse[G[_], I, A](implicit ev0: Traverse[G]): IndexedTraversal_[I, G[(I, A)], G[A], A, A] =
    IndexedTraversal_.fromTraverse[G, I, A, A]

  def wander[I, S, A](itr: Rank2TypeLensLikeWithIndex[I, S, S, A, A]): IndexedTraversal[I, S, A] =
    IndexedTraversal_.wander[I, S, S, A, A](itr)
}
