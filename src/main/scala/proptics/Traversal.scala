package proptics

import cats.data.{Const, Nested, State}
import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, Order, Traverse}
import proptics.IndexedTraversal.wander
import proptics.Lens.liftOptic
import proptics.internal.Wander.wanderStar
import proptics.internal.heyting.HeytingInstances._
import proptics.internal.{Traversing, Wander}
import proptics.newtype._
import proptics.profunctor.Star
import proptics.rank2types.{LensLikeIndexedTraversal, Rank2TypeTraversalLike}
import proptics.syntax.FunctionSyntax._
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.Function.{const, uncurried}
import scala.reflect.ClassTag

/**
 *
 * @tparam S the source of a [[Traversal]]
 * @tparam T the modified source of a [[Traversal]]
 * @tparam A the target of a [[Traversal]]
 * @tparam B the modified target of a [[Traversal]]
 */
abstract class Traversal[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]

  def over(f: A => B): S => T = self(f)

  def set(b: B): S => T = over(const(b))

  def view(s: S)(implicit ev: Monoid[A]): List[A] = foldMap(List(_))(s)

  def viewAll(s: S)(implicit ev: Monoid[A]): A = foldMap(identity)(s)

  def overF[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T]

  def foldMap[R: Monoid](f: A => R)(s: S): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(identity)(s)

  def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(ev.pure)(s)

  def traverse_[F[_], R](f: A => F[R])(s: S)(implicit ev: Applicative[F]): F[Unit] =
    foldr[F[Unit]](a => ev.void(f(a)) *> _)(ev.pure(()))(s)

  def foldr[R](f: A => R => R)(r: R)(s: S): R = foldMap(Endo[* => *, R] _ compose f)(s).runEndo(r)

  def foldl[R](f: R => A => R)(r: R)(s: S): R =
    foldMap(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip)(s).runDual.runEndo(r)

  def sum(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Additive[A], A](identity)(s)

  def product(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Multiplicative[A], A](identity)(s)

  def all(f: A => Boolean)(s: S): Boolean = allOf(f)(s)

  def allOf[R: Heyting](f: A => R)(s: S): R = foldMapNewtype[Conj[R], R](f)(s)

  def and(s: S)(implicit ev: Heyting[A]): A = allOf(identity[A])(s)

  def or(s: S)(implicit ev: Heyting[A]): A = anyOf[Id, A](identity[A])(s)

  def exists(f: A => Boolean)(s: S): Boolean = anyOf[Disj, Boolean](f)(s)

  def anyOf[F[_], R: Heyting](f: A => R)(s: S): R = foldMapNewtype[Disj[R], R](f)(s)

  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  def length(s: S): Int = foldMap(const(1))(s)

  def has[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

  def hasNot[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def find(f: A => Boolean)(s: S): Option[A] = foldr[Option[A]](a => _.fold(if (f(a)) a.some else None)(Some[A]))(None)(s)

  def first(s: S): Option[A] = preview(s)

  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](_.some)(s)

  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  def toList(s: S)(implicit ev: Monoid[A]): List[A] = view(s)

  def use[M[_]](implicit ev0: MonadState[M, S], ev1: Monoid[A]): M[List[A]] = ev0.inspect(view)

  def positions(implicit ev0: Applicative[State[Int, *]], ev1: State[Int, A]): IndexedTraversal[Int, S, T, A, B] = {
    wander(new LensLikeIndexedTraversal[Int, S, T, A, B] {
      override def apply[F[_]](f: Int => A => F[B])(implicit ev2: Applicative[F]): S => F[T] = s => {
        val starNested: Star[Nested[State[Int, *], F, *], A, B] = Star((a: A) => {
          val composed = (ev1.get, ev0.pure(a)).mapN(uncurried(f)) <* ev1.modify(_ + 1)

          Nested(composed)
        })

        val star: Star[Nested[State[Int, *], F, *], S, T] = self(starNested)
        val state: State[Int, F[T]] = star.runStar(s).value

        state.runA(0).value
      }
    })
  }

  private def hasOrHasnt[R: Heyting](s: S)(r: R): R = foldMap(const(Disj(r)))(s).runDisj

  private def foldMapNewtype[F: Monoid, R](f: A => R)(s: S)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(ev.wrap _ compose f)(s))

  private def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
    foldr[Option[A]](a => op => f(a, op.getOrElse(a)).some)(None)(s)
}

object Traversal {
  private[proptics] def apply[S, T, A, B](lensLikeTraversal: Rank2TypeTraversalLike[S, T, A, B]): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Wander[P]): P[S, T] = lensLikeTraversal(pab)

    override def overF[F[_]](f: A => F[B])(s: S)(implicit ev1: Applicative[F]): F[T] =
      lensLikeTraversal[Star[F, *, *]](Star(f)).runStar(s)
  }

  def apply[S, T, A, B](get: S => A)(_set: S => B => T): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, A, B] {
        override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] =
          s => ev.map(f(get(s)))(_set(s))
      }

      ev.wander(traversing)(pab)
    }

    override def overF[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] =
      ev.map(f(get(s)))(_set(s)(_))
  }

  def apply[S, T, A, B](to: S => (A, B => T)): Traversal[S, T, A, B] =
    Traversal(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] =
        liftOptic(to)(ev)(pab)
    })

  def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): Traversal[G[A], G[B], A, B] =
    Traversal(new Rank2TypeTraversalLike[G[A], G[B], A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev1: Wander[P]): P[G[A], G[B]] = {
        val traversing = new Traversing[G[A], G[B], A, B] {
          override def apply[F[_]](f: A => F[B])(implicit ev2: Applicative[F]): G[A] => F[G[B]] =
            ev0.traverse[F, A, B](_)(f)
        }

        ev1.wander(traversing)(pab)
      }
    })
}

object Traversal_ {
  def apply[S, A](get: S => A)(set: S => A => S): Traversal_[S, A] = Traversal(get)(set)

  def apply[S, A](to: S => (A, A => S)): Traversal_[S, A] = Traversal(to)

  def fromTraverse[G[_], A](implicit ev: Traverse[G]): Traversal[G[A], G[A], A, A] = Traversal.fromTraverse
}
