package proptics

import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}
import cats.data.Const
import cats.instances.int._
import cats.instances.list._
import cats.instances.function._
import cats.mtl.MonadState
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, Order, Traverse}
import proptics.IndexedLens_.liftIndexedOptic
import proptics.instances.boolean._
import proptics.internal._
import proptics.newtype._
import proptics.profunctor.Star
import proptics.rank2types.{Rank2TypeIndexedTraversalLike, Rank2TypeLensLikeWithIndex, Rank2TypeTraversalLike, Traversing}
import proptics.syntax.function._
import proptics.syntax.tuple._
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
  * An [[IndexedTraversal_]] is an indexed optic constrained with [[Wander]] [[cats.arrow.Profunctor]]
  *
  * @tparam I the index of an [[IndexedTraversal_]]
  * @tparam S the source of an [[IndexedTraversal_]]
  * @tparam T the modified source of an [[IndexedTraversal_]]
  * @tparam A the foci of an [[IndexedTraversal_]]
  * @tparam B the modified foci of an [[IndexedTraversal_]]
  */
abstract class IndexedTraversal_[I, S, T, A, B] extends Serializable { self =>
  def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T]

  /** collect all the foci and indices of an [[IndexedTraversal_]] into a [[List]] */
  def viewAll(s: S): List[(I, A)] = foldMap(s)(List(_))

  /** view the first focus and index of an [[IndexedTraversal_]], if there is any  */
  def preview(s: S): Option[(I, A)] = foldMapNewtype[First[(I, A)], Option[(I, A)]](s)(_.some)

  /** set the modified foci of an [[IndexedTraversal_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the foci type of an [[IndexedTraversal_]] using a function, resulting in a change of type to the full structure  */
  def over(f: ((I, A)) => B): S => T = self(Indexed(f))

  /** synonym for [[traverse]], flipped  */
  def overF[F[_]: Applicative](f: ((I, A)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify each focus of an [[IndexedTraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure  */
  def traverse[F[_]: Applicative](s: S)(f: ((I, A)) => F[B]): F[T] =
    self[Star[F, *, *]](Indexed(Star[F, (I, A), B](f))).runStar(s)

  /** map each focus and index of an [[IndexedTraversal_] to a [[Monoid]], and combine the results */
  def foldMap[R: Monoid](s: S)(f: ((I, A)) => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** fold the foci and indices of an [[IndexedTraversal_]] using a binary operator, going right to left */
  def foldr[R](s: S)(r: R)(f: ((I, A)) => R => R): R = foldMap(s)(Endo[* => *, R] _ compose f).runEndo(r)

  /** fold the foci and indices of an [[IndexedTraversal_]] using a binary operator, going left to right */
  def foldl[R](s: S)(r: R)(f: R => ((I, A)) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip).runDual.runEndo(r)

  /** evaluate each focus and index of an [[IndexedTraversal_]] from left to right, and ignore the results structure  */
  def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  /** map each focus and index of an [[IndexedTraversal_]] to an effect, from left to right, and ignore the results */
  def traverse_[F[_], R](s: S)(f: ((I, A)) => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldr[F[Unit]](s)(ev.pure(()))(ia => ev.void(f(ia)) *> _)

  /** the sum of all foci of an [[IndexedTraversal_]] */
  def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMapNewtype[Additive[A], A](s)(_._2)

  /** the product of all foci of an [[IndexedTraversal_]] */
  def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMapNewtype[Multiplicative[A], A](s)(_._2)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedTraversal_]] */
  def forall(f: ((I, A)) => Boolean): S => Boolean = s => forall(s)(f)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: ((I, A)) => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** return the result of a conjunction of all foci of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(_._2)

  /** return the result of a disjunction of all foci of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  def or(s: S)(implicit ev: Heyting[A]): A = any[Id, A](s)(_._2)

  /** test whether a predicate holds for any focus and index of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  def any[F[_], R: Heyting](s: S)(f: ((I, A)) => R): R = foldMapNewtype[Disj[R], R](s)(f)

  /** test whether a predicate holds for any focus and index of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  def exists(f: ((I, A)) => Boolean): S => Boolean = s => any[Disj, Boolean](s)(f)

  /** test whether a predicate does not hold for any focus and index of an [[IndexedTraversal_]] */
  def notExists(f: ((I, A)) => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a focus at specific index of an [[IndexedTraversal_]] contains a given value */
  def contains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedTraversal_]] does not contain a given value */
  def notContains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = !contains(s)(a)

  /** check if the [[IndexedTraversal_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[IndexedTraversal_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of an [[IndexedTraversal_]] */
  def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of an [[IndexedTraversal_]] that satisfies a predicate, if there is any */
  def find(f: ((I, A)) => Boolean): S => Option[A] = s => foldr[Option[A]](s)(None)(ia => _.fold(if (f(ia)) ia._2.some else None)(Some[A]))

  /** synonym for [[preview]] */
  def first(s: S): Option[(I, A)] = preview(s)

  /** find the last focus and index of an [[IndexedTraversal_]] that satisfies a predicate, if there is any */
  def last(s: S): Option[(I, A)] = foldMapNewtype[Last[(I, A)], Option[(I, A)]](s)(_.some)

  /** the minimum of all foci of an [[IndexedTraversal_]], if there is any */
  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of an [[IndexedTraversal_]], if there is any */
  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of an [[IndexedTraversal_]] into an [[Array]] */
  def toArray[AA >: (I, A)](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[(I, A)]): Array[AA] = toList(s).toArray

  /** synonym to [[viewAll]] */
  def toList(s: S): List[(I, A)] = viewAll(s)

  /** view the focus and the index of an [[IndexedTraversal_]] in the state of a monad */
  def use[M[_]](implicit ev: MonadState[M, S]): M[List[(I, A)]] = ev.inspect(viewAll)

  /** synonym to [[asTraversal]] */
  def unIndex: Traversal_[S, T, A, B] = asTraversal

  /** transform an [[IndexedTraversal_]] to a [[Traversal_]] */
  def asTraversal: Traversal_[S, T, A, B] =
    Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] =
        self(Indexed(ev.dimap[A, B, (I, A), B](pab)(_._2)(identity)))
    })

  /** compose an [[IndexedTraversal_]] with an [[IndexedLens_]] */
  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (I, C), D] = new Traversing[S, T, (I, C), D] {
        override def apply[F[_]](f: ((I, C)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (_, a) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose an [[IndexedTraversal_]] with an [[AnIndexedLens_]] */
  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = self compose other.asIndexedLens

  /** compose an [[IndexedTraversal_]] with an [[IndexedTraversal_]] */
  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (I, C), D] = new Traversing[S, T, (I, C), D] {
        override def apply[F[_]](f: ((I, C)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (_, a) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedSetter_]] */
  def compose[C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B](other(indexed) compose Tuple2._2))
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedGetter_]] */
  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = self compose other.asIndexedFold

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]] */
  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] = {
      val runForget = other(indexed).runForget

      Forget(self.foldMap(_)(runForget compose Tuple2._2))
    }
  }

  private def foldMapNewtype[F: Monoid, R](s: S)(f: ((I, A)) => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldr[Option[A]](s)(None)(pair => _.map(f(pair._2, _)))
}

object IndexedTraversal_ {

  /** create a polymorphic [[IndexedTraversal_]] from Rank2TypeIndexedTraversalLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedTraversalLike[I, S, T, A, B]): IndexedTraversal_[I, S, T, A, B] = new IndexedTraversal_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = f(indexed)
  }

  /** create a polymorphic [[IndexedTraversal_]] from a getter/setter pair */
  def apply[I, S, T, A, B](get: S => (I, A))(_set: S => B => T): IndexedTraversal_[I, S, T, A, B] = new IndexedTraversal_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (I, A), B] = new Traversing[S, T, (I, A), B] {
        override def apply[F[_]](f: ((I, A)) => F[B])(s: S)(implicit ev: Applicative[F]): F[T] =
          ev.map(f(get(s)))(_set(s))
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** create a polymorphic [[IndexedTraversal_]] from a combined getter/setter */
  def traversal[I, S, T, A, B](to: S => ((I, A), B => T)): IndexedTraversal_[I, S, T, A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(indexed.runIndex)
    })

  /** create a polymorphic [[IndexedTraversal_]] from a [[Traverse]] */
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

  /** create a polymorphic [[IndexedTraversal_]] from a rank 2 type traversal function */
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

  /** create a momnomorphic [[IndexedTraversal]] from a getter/setter pair */
  def apply[I, S, A](get: S => (I, A))(set: S => A => S): IndexedTraversal[I, S, A] = IndexedTraversal_(get)(set)

  /** create a monomorphic [[IndexedTraversal]] from a combined getter/setter. synonym to apply */
  def traversal[I, S, A](to: S => ((I, A), A => S)): IndexedTraversal[I, S, A] = IndexedTraversal_.traversal(to)

  /** create a momnomorphic [[IndexedTraversal_]] from a [[Traverse]] */
  def fromTraverse[G[_], I, A](implicit ev0: Traverse[G]): IndexedTraversal_[I, G[(I, A)], G[A], A, A] =
    IndexedTraversal_.fromTraverse[G, I, A, A]

  /** create a monomorphic [[IndexedTraversal]] from a rank 2 type traversal function */
  def wander[I, S, A](itr: Rank2TypeLensLikeWithIndex[I, S, S, A, A]): IndexedTraversal[I, S, A] =
    IndexedTraversal_.wander[I, S, S, A, A](itr)
}
