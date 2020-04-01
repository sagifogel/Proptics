package proptics

import cats.data.Const
import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, Order, Traverse}
import proptics.instances.BooleanInstances._
import proptics.internal.{Bazaar, RunBazaar}
import proptics.newtype._
import proptics.syntax.FunctionSyntax._
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
 * A [[Traversal_]] with fixed type [[Bazaar]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[ATraversal_]]
 * @tparam T the modified source of a [[ATraversal_]]
 * @tparam A the target of a [[ATraversal_]]
 * @tparam B the modified target of a [[ATraversal_]]
 */
abstract class ATraversal_[S, T, A, B] { self =>
  private[proptics] def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T]

  def view(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  def viewAll(s: S)(implicit ev: Monoid[A]): List[A] = foldMap(s)(List(_))

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = s => traverse[Id](s)(f)

  def overF[F[_] : Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[G[_]](s: S)(f: A => G[B])(implicit ev: Applicative[G]): G[T]

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

  private def hasOrHasnt[R: Heyting](s: S)(r: R): R = foldMap(s)(const(Disj(r))).runDisj

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
    foldr[Option[A]](s)(None)(a => op => f(a, op.getOrElse(a)).some)
}

object ATraversal_ {
  def apply[S, T, A, B](runBazaar: RunBazaar[* => *, A, B, S, T]): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
    override private[proptics] def apply(bazaar: Bazaar[Function, A, B, A, B]) = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = runBazaar(pafb)(s)
      }
    }

    override def traverse[G[_]](s: S)(f: A => G[B])(implicit ev: Applicative[G]): G[T] = runBazaar(f)(s)
  }

  def apply[S, T, A, B](get: S => A)(_set: B => S => T): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = {
          val fb = pafb(get(s))
          ev.map(fb)(_set(_)(s))
        }
      }
    }

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] =
      ev.map(f(get(s)))(set(_)(s))
  }

  def apply[P[_, _], S, T, A, B](to: S => (A, B => T)): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = traverse(s)(pafb)
      }
    }

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev1: Applicative[F]): F[T] = {
      val (a, b2t) = to(s)

      ev1.map(f(a))(b2t)
    }
  }

  def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): ATraversal_[G[A], G[B], A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, G[A], G[B]] {
      override def apply[F[_]](pafb: A => F[B])(s: G[A])(implicit ev: Applicative[F]): F[G[B]] =
        ev0.traverse(s)(pafb)
    })
}

object ATraversal {
  def apply[S, A](get: S => A)(set: A => S => S): ATraversal[S, A] = ATraversal_(get)(set)

  def apply[S, A](to: S => (A, A => S)): ATraversal[S, A] = ATraversal_(to)

  def fromTraverse[G[_] : Traverse, A]: ATraversal[G[A], A] = ATraversal_.fromTraverse
}
