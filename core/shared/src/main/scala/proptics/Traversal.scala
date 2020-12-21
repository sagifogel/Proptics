package proptics

import scala.Function.const
import scala.reflect.ClassTag

import cats.data.{Const, Nested, State}
import cats.syntax.apply._
import cats.syntax.bitraverse._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Bitraverse, Eq, Monoid, Order, Traverse}
import spire.algebra.lattice.Heyting
import spire.algebra.{MultiplicativeMonoid, Semiring}
import spire.std.boolean._

import proptics.IndexedTraversal_.wander
import proptics.Lens_.liftOptic
import proptics.internal._
import proptics.newtype._
import proptics.profunctor.Wander._
import proptics.profunctor.{Star, Traversing, Wander}
import proptics.rank2types.{LensLike, LensLikeWithIndex, Rank2TypeTraversalLike}
import proptics.syntax.function._
import proptics.syntax.star._
import proptics.syntax.traversal._

/** @tparam S the source of a [[Traversal_]]
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

  /** modify the foci type of a [[Traversal_]] using a function, resulting in a change of type to the full structure */
  def over(f: A => B): S => T = self(f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify each focus of a [[Traversal_]] using a Functor, resulting in a change of type to the full structure */
  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  /** map each focus of a [[Traversal_] to a Monoid, and combine the results */
  def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** fold the foci of a [[Traversal_]] using a [[Monoid]] */
  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  /** fold the foci of a [[Traversal_]] using a binary operator, going right to left */
  def foldRight[R](s: S)(r: R)(f: (A, R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of a [[Traversal_]] using a binary operator, going left to right */
  def foldLeft[R](s: S)(r: R)(f: (R, A) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** evaluate each  focus of a [[Traversal_]] from left to right, and ignore the results structure */
  def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  /** map each focus of a [[Traversal_]] to an effect, from left to right, and ignore the results */
  def traverse_[F[_], R](s: S)(f: A => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldLeft[F[Unit]](s)(ev.pure(()))((b, a) => ev.void(f(a)) *> b)

  /** the sum of all foci of a [[Traversal_]] */
  def sum(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Additive[A], A](s)(identity)

  /** the product of all foci of a [[Traversal_]] */
  def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMapNewtype[Multiplicative[A], A](s)(identity)

  /** test whether there is no focus or a predicate holds for all foci of a [[Traversal_]] */
  def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for all foci of a [[Traversal_]], using a Heyting algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** return the result of a conjunction of all foci of a [[Traversal_]], using a Heyting algebra */
  def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(identity)

  /** return the result of a disjunction of all foci of a [[Traversal_]], using a Heyting algebra */
  def or(s: S)(implicit ev: Heyting[A]): A = any[A](s)(identity)

  /** test whether a predicate holds for any focus of a [[Traversal_]], using a Heyting algebra */
  def any[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Disj[R], R](s)(f)

  /** test whether a predicate holds for any foci of a [[Traversal_]] */
  def exists(f: A => Boolean): S => Boolean = any[Boolean](_)(f)

  /** test whether a predicate does not hold for the foci of a [[Traversal_]] */
  def notExists(f: A => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a [[Traversal_]] contains a specific focus */
  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether a [[Traversal_]] does not contain a specific focus */
  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** check if the [[Traversal_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[Traversal_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of a [[Traversal_]] */
  def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of a [[Traversal_]] that satisfies a predicate, if there is any */
  def find(f: A => Boolean): S => Option[A] =
    foldRight[Option[A]](_)(None)((a, b) => b.fold(if (f(a)) a.some else None)(Some[A]))

  /** synonym for [[preview]] */
  def first(s: S): Option[A] = preview(s)

  /** find the last focus of a [[Traversal_]], if there is any */
  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](s)(_.some)

  /** the minimum of all foci of a [[Traversal_]], if there is any */
  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of a [[Traversal_]], if there is any */
  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of a [[Traversal_]] into an Array */
  def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  /** synonym to [[viewAll]] */
  def toList(s: S): List[A] = viewAll(s)

  /** collect all the foci of a [[Traversal_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, List[A]] = ev.inspect(viewAll)

  /** convert a [[Traversal_]] to an [[IndexedTraversal_]] by using the integer positions as indices */
  def asIndexableTraversal(implicit ev0: Applicative[State[Int, *]]): IndexedTraversal_[Int, S, T, A, B] =
    wander(new LensLikeWithIndex[Int, S, T, A, B] {
      override def apply[F[_]](f: ((Int, A)) => F[B])(implicit ev1: Applicative[F]): S => F[T] = s => {
        val state: State[Int, Unit] = State.apply[Int, Unit](i => (i, ()))
        val starNested: Star[Nested[State[Int, *], F, *], A, B] = Star { a =>
          val composed = (state.get, ev0.pure(a)).mapN((i, a) => f((i, a))) <* state.modify(_ + 1)

          Nested(composed)
        }

        self(starNested)
          .runStar(s)
          .value
          .runA(0)
          .value
      }
    })

  /** transform a [[Traversal_]] to a [[Fold_]] */
  def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget))
  }

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
    foldRight[Option[A]](s)(None)((a, op) => f(a, op.getOrElse(a)).some)
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
  def traversal[S, T, A, B](to: S => (A, B => T)): Traversal_[S, T, A, B] = Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = liftOptic(to)(ev)(pab)
  })

  /** traverse elements of a [[Traversal_]] that satisfy a predicate */
  def filter[A](predicate: A => Boolean): Traversal_[A, A, A, A] =
    Traversal_[A, A, A, A](new Rank2TypeTraversalLike[A, A, A, A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev0: Wander[P]): P[A, A] = {
        val traversing = new Traversing[A, A, A, A] {
          override def apply[F[_]](f: A => F[A])(s: A)(implicit ev1: Applicative[F]): F[A] =
            if (predicate(s)) f(s) else ev1.pure(s)
        }

        ev0.wander(traversing)(pab)
      }
    })

  /** create a polymorphic [[Traversal_]] from Traverse */
  def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): Traversal_[G[A], G[B], A, B] = Traversal_(new Rank2TypeTraversalLike[G[A], G[B], A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev1: Wander[P]): P[G[A], G[B]] = {
      val traversing = new Traversing[G[A], G[B], A, B] {
        override def apply[F[_]](f: A => F[B])(s: G[A])(implicit ev2: Applicative[F]): F[G[B]] =
          ev0.traverse[F, A, B](s)(f)
      }

      ev1.wander(traversing)(pab)
    }
  })

  /** create a polymorphic [[Traversal_]] from [[Bazaar]] */
  def fromBazaar[S, T, A, B](bazaar: Bazaar[* => *, A, B, S, T]): Traversal_[S, T, A, B] =
    Traversal_[S, T, A, B](new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
        val traversing = new Traversing[S, T, A, B] {
          override def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = bazaar.runBazaar(f)(s)
        }

        ev.wander(traversing)(pab)
      }
    })

  /** traverse both parts of a Bitraverse with matching types */
  def both[G[_, _]: Bitraverse, A, B]: Traversal_[G[A, A], G[B, B], A, B] =
    Traversal_(new Rank2TypeTraversalLike[G[A, A], G[B, B], A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[G[A, A], G[B, B]] = {
        val traversing = new Traversing[G[A, A], G[B, B], A, B] {
          override def apply[F[_]](f: A => F[B])(s: G[A, A])(implicit ev: Applicative[F]): F[G[B, B]] =
            s.bitraverse(f, f)
        }

        ev.wander(traversing)(pab)
      }
    })

  /** create a polymorphic [[Traversal_]] from a rank 2 type traversal function */
  def wander[S, T, A, B](lensLike: LensLike[S, T, A, B]): Traversal_[S, T, A, B] =
    Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Wander[P]): P[S, T] = {
        def traversing: Traversing[S, T, A, B] = new Traversing[S, T, A, B] {
          override def apply[F[_]](f: A => F[B])(s: S)(implicit ev1: Applicative[F]): F[T] = lensLike[F](f)(ev1)(s)
        }

        ev0.wander(traversing)(pab)
      }
    })

  /** polymorphic identity of a [[Traversal_]] */
  def id[S, T]: Traversal_[S, T, S, T] = Traversal_(identity[S] _)(const(identity[T]))
}

object Traversal {
  /** create a monomorphic [[Traversal]] from Rank2TypeTraversalLike encoding */
  def fromTraversal[S, A](lensLikeTraversal: Rank2TypeTraversalLike[S, S, A, A]): Traversal[S, A] =
    Traversal_[S, S, A, A](lensLikeTraversal)

  /** create a monomorphic [[Traversal]] from a getter/setter pair */
  def apply[S, A](get: S => A)(set: S => A => S): Traversal[S, A] = Traversal_(get)(set)

  /** create a monomorphic [[Traversal]] from a combined getter/setter */
  def traversal[S, A](to: S => (A, A => S)): Traversal[S, A] = Traversal_.traversal(to)

  /** traverse elements of a [[Traversal]], that satisfy a predicate */
  def filter[A](predicate: A => Boolean): Traversal[A, A] = Traversal_.filter(predicate)

  /** traverse elements of a [[Traversal]], by taking the first element of another [[Traversal]] and using it as a filter */
  def filter[A, B](traversal: Traversal[A, B]): Fold[A, A] = Fold.filter(traversal.asFold)

  /** traverse elements of an [[Traversal]], by taking the first element of another [[AffineTraversal]] and using it as a filter */
  def filter[A, B](traversal: AffineTraversal[A, B]): Fold[A, A] = Fold.filter(traversal.asFold)

  /** traverse elements of a [[Traversal]], by taking the first element of a [[Fold]] and using it as a filter */
  def filter[A, B](fold: Fold[A, B]): Traversal[A, A] =
    Traversal_[A, A, A, A](new Rank2TypeTraversalLike[A, A, A, A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev0: Wander[P]): P[A, A] = {
        val traversing = new Traversing[A, A, A, A] {
          override def apply[F[_]](f: A => F[A])(s: A)(implicit ev1: Applicative[F]): F[A] =
            fold.preview(s).fold(ev1.pure(s))(const(f(s)))
        }

        ev0.wander(traversing)(pab)
      }
    })

  /** create a monomorphic [[Traversal]] from a [[Traverse]] */
  def fromTraverse[F[_]: Traverse, A]: Traversal[F[A], A] = Traversal_.fromTraverse

  /** create a monomorphic [[Traversal]] from a [[Bazaar]] */
  def fromBazaar[S, A](bazaar: Bazaar[* => *, A, A, S, S]): Traversal[S, A] = Traversal_.fromBazaar(bazaar)

  /** traverse both parts of a Bitraverse with matching types */
  def both[G[_, _]: Bitraverse, A]: Traversal[G[A, A], A] = Traversal_.both[G, A, A]

  /** create a monomorphic [[Traversal]] from a rank 2 type traversal function */
  def wander[S, A](lensLike: LensLike[S, S, A, A]): Traversal[S, A] =
    Traversal_.wander[S, S, A, A](lensLike)

  /** monomorphic identity of a [[Traversal]] */
  def id[S]: Traversal[S, S] = Traversal_.id[S, S]

  /** create a monomorphic [[Traversal]] that narrows the focus to a single element */
  def element[F[_]: Traverse, A](i: Int): Traversal[F[A], A] = Traversal.fromTraverse[F, A].element(i)

  /** create a monomorphic [[Traversal]] that selects the first n elements of a Traverse */
  def take[F[_]: Traverse, A](i: Int): Traversal[F[A], A] = Traversal.fromTraverse[F, A].take(i)

  /** create a monomorphic [[Traversal]] that selects all elements of a Traverse except the first n ones */
  def drop[F[_]: Traverse, A](i: Int): Traversal[F[A], A] = Traversal.fromTraverse[F, A].drop(i)

  /** create a monomorphic [[Traversal]] that takes the longest prefix of elements of a Traverse that satisfy a predicate */
  def takeWhile[G[_]: Traverse, A](predicate: A => Boolean): Traversal[G[A], A] =
    Traversal.fromTraverse[G, A].takeWhile(predicate)

  /** drop longest prefix of elements of a Traverse that satisfy a predicate */
  def dropWhile[G[_]: Traverse, A](predicate: A => Boolean): Traversal[G[A], A] =
    Traversal.fromTraverse[G, A].dropWhile(predicate)
}
