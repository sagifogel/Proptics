package proptics

import cats.data.{Const, Nested, State}
import cats.implicits._
import cats.{Applicative, Id, Monoid, Traverse}
import proptics.IndexedTraversal.wander
import proptics.Lens.liftOptic
import proptics.internal.Wander.wanderStar
import proptics.internal.{Traversing, Wander}
import proptics.rank2types.{LensLikeIndexedTraversal, Rank2TypeTraversalLike}
import proptics.newtype.{Additive, Conj, Disj, Newtype}
import proptics.internal.heyting.HeytingInstances._
import proptics.profunctor.Star
import proptics.newtype.Newtype

import scala.Function.{const, uncurried}

/**
 *
 * @tparam S the source of a [[Traversal]]
 * @tparam T the modified source of a [[Traversal]]
 * @tparam A the target of a [[Traversal]]
 * @tparam B the modified target of a [[Traversal]]
 */
abstract class Traversal[S, T, A, B] { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]

  def over(f: A => B): S => T = self(f)

  def set(b: B): S => T = over(const(b))

  def view(s: S)(implicit ev: Monoid[A]): List[A] = foldMap(List(_))(s)

  def overF[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T]

  def foldMap[R: Monoid](f: A => R)(s: S): R = overF[Const[R, ?]](a => Const(f(a)))(s).getConst

  def sum(s: S)(implicit ev: Monoid[A]): A = foldMap(Additive[A])(s).runAdditive

  def all(f: A => Boolean)(s: S): Boolean = allOf(f)(s)

  def allOf[R](f: A => R)(s: S)(implicit ev: Monoid[Conj[R]]): R = allOrAnyOf[Conj, R](f)(s)

  def and(s: S)(implicit ev: Monoid[Conj[A]]): A = allOf(identity[A])(s)

  def or(s: S)(implicit ev: Monoid[Disj[A]]): A = anyOf[Id, A](identity[A])(s)

  def exists[R](f: A => Boolean)(s: S): Boolean = anyOf[Disj, Boolean](f)(s)

  def anyOf[F[_], R](f: A => R)(s: S)(implicit ev0: Monoid[Disj[R]]): R = allOrAnyOf[Disj, R](f)(s)

  private def allOrAnyOf[F[_], R](f: A => R)(s: S)(implicit ev0: Monoid[F[R]], ev1: Newtype.Aux[F[R], R]): R =
    ev1.unwrap(foldMap(a => ev1.wrap(f(a)))(s))

  def length(s: S): Int = foldMap(const(1))(s)

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
}

object Traversal {
  private[proptics] def apply[S, T, A, B](lensLikeTraversal: Rank2TypeTraversalLike[S, T, A, B]): Traversal[S, T, A, B] = new Traversal[S, T, A, B] { self =>
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