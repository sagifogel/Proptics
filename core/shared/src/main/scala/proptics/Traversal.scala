package proptics

import cats.data.{Const, Nested, State}
import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, Order, Traverse}
import proptics.IndexedTraversal_.wander
import proptics.Lens_.liftOptic
import proptics.instances.BooleanInstances._
import proptics.internal.Wander.wanderStar
import proptics.internal._
import proptics.newtype._
import proptics.profunctor.Star
import proptics.rank2types.{Rank2TypeLensLikeWithIndex, Rank2TypeTraversalLike}
import proptics.syntax.function._
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
  *
  * @tparam S the source of a [[Traversal_]]
  * @tparam T the modified source of a [[Traversal_]]
  * @tparam A the target of a [[Traversal_]]
  * @tparam B the modified target of a [[Traversal_]]
  */
abstract class Traversal_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]

  def view(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  def viewAll(s: S)(implicit ev: Monoid[A]): List[A] = foldMap(s)(List(_))

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = self(f)

  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  def foldr[R](s: S)(r: R)(f: A => R => R): R = foldMap(s)(Endo[* => *, R] _ compose f).runEndo(r)

  def foldl[R](s: S)(r: R)(f: R => A => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip).runDual.runEndo(r)

  def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  def traverse_[F[_], R](s: S)(f: A => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldr[F[Unit]](s)(ev.pure(()))(a => ev.void(f(a)) *> _)

  def sum(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Additive[A], A](s)(identity)

  def product(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Multiplicative[A], A](s)(identity)

  def all(f: A => Boolean): S => Boolean = allOf(_)(f)

  def allOf[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  def and(s: S)(implicit ev: Heyting[A]): A = allOf(s)(identity)

  def or(s: S)(implicit ev: Heyting[A]): A = anyOf[Id, A](s)(identity)

  def exists(f: A => Boolean): S => Boolean = anyOf[Disj, Boolean](_)(f)

  def anyOf[F[_], R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Disj[R], R](s)(f)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def length(s: S): Int = foldMap(s)(const(1))

  def has[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

  def hasNot[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)

  def find(f: A => Boolean): S => Option[A] =
    foldr[Option[A]](_)(None)(a => _.fold(if (f(a)) a.some else None)(Some[A]))

  def first(s: S): Option[A] = preview(s)

  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](s)(_.some)

  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  def toList(s: S)(implicit ev: Monoid[A]): List[A] = viewAll(s)

  def use[M[_]](implicit ev0: MonadState[M, S], ev1: Monoid[A]): M[List[A]] = ev0.inspect(viewAll)

  def zipWith[F[_]](f: A => A => B): S => S => T = self(Zipping(f)).runZipping

  def positions(implicit ev0: Applicative[State[Int, *]], ev1: State[Int, A]): IndexedTraversal_[Int, S, T, A, B] =
    wander(new Rank2TypeLensLikeWithIndex[Int, S, T, A, B] {
      override def apply[F[_]](f: ((Int, A)) => F[B])(implicit ev2: Applicative[F]): S => F[T] = s => {
        val starNested: Star[Nested[State[Int, *], F, *], A, B] = Star { a =>
          val composed = (ev1.get, ev0.pure(a)).mapN((i, a) => f((i, a))) <* ev1.modify(_ + 1)

          Nested(composed)
        }

        val star: Star[Nested[State[Int, *], F, *], S, T] = self(starNested)
        val state: State[Int, F[T]] = star.runStar(s).value

        state.runA(0).value
      }
    })

  def compose[C, D](other: Iso_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asIso

  def compose[C, D](other: Lens_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  def compose[C, D](other: ALens_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asLens

  def compose[C, D](other: Prism_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  def compose[C, D](other: APrism_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asPrism

  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] = {
        self.traverse(s)(other.traverse(_)(pafb))
      }
    })

  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold_

  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  private def hasOrHasnt[R: Heyting](s: S)(r: R): R = foldMap(s)(const(Disj(r))).runDisj

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
    foldr[Option[A]](s)(None)(a => op => f(a, op.getOrElse(a)).some)
}

object Traversal_ {
  private[proptics] def apply[S, T, A, B](lensLikeTraversal: Rank2TypeTraversalLike[S, T, A, B]): Traversal_[S, T, A, B] = new Traversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Wander[P]): P[S, T] = lensLikeTraversal(pab)
  }

  def apply[S, T, A, B](get: S => A)(_set: S => B => T): Traversal_[S, T, A, B] = new Traversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, A, B] {
        override def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = ev.map(f(get(s)))(_set(s))
      }

      ev.wander(traversing)(pab)
    }
  }

  def apply[S, T, A, B](to: S => (A, B => T)): Traversal_[S, T, A, B] =
    Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = liftOptic(to)(ev)(pab)
    })

  def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): Traversal_[G[A], G[B], A, B] =
    Traversal_(new Rank2TypeTraversalLike[G[A], G[B], A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev1: Wander[P]): P[G[A], G[B]] = {
        val traversing = new Traversing[G[A], G[B], A, B] {
          override def apply[F[_]](f: A => F[B])(s: G[A])(implicit ev2: Applicative[F]): F[G[B]] =
            ev0.traverse[F, A, B](s)(f)
        }

        ev1.wander(traversing)(pab)
      }
    })
}

object Traversal {
  def apply[S, A](get: S => A)(set: S => A => S): Traversal[S, A] = Traversal_(get)(set)

  def apply[S, A](to: S => (A, A => S)): Traversal[S, A] = Traversal_(to)

  def fromTraverse[G[_], A](implicit ev: Traverse[G]): Traversal_[G[A], G[A], A, A] = Traversal_.fromTraverse
}
