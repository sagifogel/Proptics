package proptics

import cats.data.{Const, Nested, State}
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Monoid, Order, Traverse}
import proptics.IndexedTraversal_.wander
import proptics.Lens_.liftOptic
import proptics.instances.boolean._
import proptics.internal.Wander.wanderStar
import proptics.internal._
import proptics.newtype._
import proptics.profunctor.Star
import proptics.rank2types.{Rank2TypeLensLikeWithIndex, Rank2TypeTraversalLike, Traversing}
import proptics.syntax.function._
import spire.algebra.lattice.Heyting
import spire.algebra.{MultiplicativeMonoid, Semiring}

import scala.Function.const
import scala.reflect.ClassTag

/**
  * @tparam S the source of a [[Traversal_]]
  * @tparam T the modified source of a [[Traversal_]]
  * @tparam A the foci of a [[Traversal_]]
  * @tparam B the modified foci of a [[Traversal_]]
  */
abstract class Traversal_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]

  /** synonym to [[fold]] */
  def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  /** collect all the foci of a [[Traversal_]] into a [[List]] */
  def viewAll(s: S): List[A] = foldMap(s)(List(_))

  /** view the first focus of a [[Traversal_]], if there is any */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  /** set the modified foci of a [[Traversal_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the foci type of a [[Prism_]] using a function, resulting in a change of type to the full structure */
  def over(f: A => B): S => T = self(f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify each focus of a [[Traversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  /** map each focus of a [[Traversal_] to a [[Monoid]], and combine the results */
  def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** fold the foci of a [[Traversal_]] using a [[Monoid]] */
  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  /** fold the foci of a [[Traversal_]] using a binary operator, going right to left */
  def foldr[R](s: S)(r: R)(f: (A, R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of a [[Traversal_]] using a binary operator, going left to right */
  def foldl[R](s: S)(r: R)(f: (R, A) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** evaluate each  focus of a [[Traversal_]] from left to right, and ignore the results structure */
  def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  /** map each focus of a [[Traversal_]] to an effect, from left to right, and ignore the results */
  def traverse_[F[_], R](s: S)(f: A => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldr[F[Unit]](s)(ev.pure(()))((a, b) => ev.void(f(a)) *> b)

  /** the sum of all foci of a [[Traversal_]] */
  def sum(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Additive[A], A](s)(identity)

  /** the product of all foci of a [[Traversal_]] */
  def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMapNewtype[Multiplicative[A], A](s)(identity)

  /** test whether there is no focus or a predicate holds for all foci of a [[Traversal_]] */
  def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for all foci of a [[Traversal_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** return the result of a conjunction of all foci of a [[Traversal_]], using a [[Heyting]] algebra */
  def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(identity)

  /** return the result of a disjunction of all foci of a [[Traversal_]], using a [[Heyting]] algebra */
  def or(s: S)(implicit ev: Heyting[A]): A = any[A](s)(identity)

  /** test whether a predicate holds for any focus of a [[Traversal_]], using a [[Heyting]] algebra */
  def any[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Disj[R], R](s)(f)

  /** test whether a predicate holds for any foci of a [[Traversal_]] */
  def exists(f: A => Boolean): S => Boolean = any[Boolean](_)(f)

  /** test whether a predicate does not hold for the foci of a [[Traversal_]] */
  def notExists(f: A => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a [[Traversal_]] contains a specific focus */
  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether a [[Traversal_]] does not contain a specific focus */
  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  /** check if the [[Traversal_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[Traversal_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of a [[Traversal_]] */
  def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of a [[Traversal_]] that satisfies a predicate, if there is any */
  def find(f: A => Boolean): S => Option[A] =
    foldr[Option[A]](_)(None)((a, b) => b.fold(if (f(a)) a.some else None)(Some[A]))

  /** synonym for [[preview]] */
  def first(s: S): Option[A] = preview(s)

  /** find the last focus of a [[Traversal_]], if there is any */
  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](s)(_.some)

  /** the minimum of all foci of a [[Traversal_]], if there is any */
  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of a [[Traversal_]], if there is any */
  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of a [[Traversal_]] into an [[Array]] */
  def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  /** synonym to [[viewAll]] */
  def toList(s: S): List[A] = viewAll(s)

  /** collect all the foci of a [[Traversal_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, List[A]] = ev.inspect(viewAll)

  /** convert a [[Traversal_]] to an [[IndexedTraversal_]] by using the integer positions as indices */
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

  /** compose a [[Traversal_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Traversal_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asIso

  /** compose a [[Traversal_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Traversal_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asLens

  /** compose a [[Traversal_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Traversal_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asPrism

  /** compose a [[Traversal_]] with an [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Traversal_]] with an [[AnAffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] = self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pab)
    }
  }

  /** compose a [[Traversal_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Traversal_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose a [[Traversal_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Traversal_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold

  /** compose a [[Traversal_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldr[Option[A]](s)(None)((a, op) => f(a, op.getOrElse(a)).some)
}

object Traversal_ {

  /** create a polymorphic [[Traversal_]] from Rank2TypeTraversalLike encoding */
  private[proptics] def apply[S, T, A, B](lensLikeTraversal: Rank2TypeTraversalLike[S, T, A, B]): Traversal_[S, T, A, B] = new Traversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Wander[P]): P[S, T] = lensLikeTraversal(pab)
  }

  /** create a polymorphic [[Traversal_]] from a getter/setter pair */
  def apply[S, T, A, B](get: S => A)(_set: S => B => T): Traversal_[S, T, A, B] = new Traversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, A, B] {
        override def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = ev.map(f(get(s)))(_set(s))
      }

      ev.wander(traversing)(pab)
    }
  }

  /** create a polymorphic [[Traversal_]] from a combined getter/setter */
  def traversal[S, T, A, B](to: S => (A, B => T)): Traversal_[S, T, A, B] =
    Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = liftOptic(to)(ev)(pab)
    })

  /** create a polymorphic [[Traversal_]] from a [[Traverse]] */
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

  /** polymorphic identity of a [[Traversal_]] */
  def id[S, T]: Traversal_[S, T, S, T] = Traversal_(identity[S] _)(const(identity[T]))
}

object Traversal {

  /** create a momnomorphic [[Traversal]] from a getter/setter pair */
  def apply[S, A](get: S => A)(set: S => A => S): Traversal[S, A] = Traversal_(get)(set)

  /** create a monomorphic [[Traversal]] from a combined getter/setter */
  def traversal[S, A](to: S => (A, A => S)): Traversal[S, A] = Traversal_.traversal(to)

  /** create a monomorphic [[Traversal]] from a [[Traverse]] */
  def fromTraverse[G[_]: Traverse, A]: Traversal[G[A], A] = Traversal_.fromTraverse

  /** monomorphic identity of a [[Traversal]] */
  def id[S]: Traversal[S, S] = Traversal_.id[S, S]
}
