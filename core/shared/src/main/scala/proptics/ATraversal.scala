package proptics

import cats.data.{Const, State}
import cats.instances.function._
import cats.instances.int._
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, Order, Traverse}
import proptics.instances.boolean._
import proptics.internal._
import proptics.newtype._
import proptics.rank2types.Traversing
import proptics.syntax.function._
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}

import scala.Function.const
import scala.reflect.ClassTag

/**
  * A [[Traversal_]] with fixed type [[Bazaar]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of a [[ATraversal_]]
  * @tparam T the modified source of a [[ATraversal_]]
  * @tparam A the foci of a [[ATraversal_]]
  * @tparam B the modified foci of a [[ATraversal_]]
  */
abstract class ATraversal_[S, T, A, B] { self =>
  private[proptics] def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T]

  /** synonym to [[fold]] */
  def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  /** collect all the foci of a [[ATraversal_]] into a [[List]] */
  def viewAll(s: S): List[A] = foldMap(s)(List(_))

  /** view the first focus of a [[ATraversal_]], if there is any  */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  /** set the modified foci of a [[ATraversal_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the foci type of a [[Prism_]] using a function, resulting in a change of type to the full structure  */
  def over(f: A => B): S => T = s => traverse[Id](s)(f)

  /** synonym for [[traverse]], flipped  */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify each focus of a [[ATraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure  */
  def traverse[G[_]](s: S)(f: A => G[B])(implicit ev: Applicative[G]): G[T]

  /** map each focus of a [[Traversal_] to a [[Monoid]], and combine the results */
  def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** fold the foci of a [[ATraversal_]] using a [[Monoid]] */
  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  /** fold the foci of a [[ATraversal_]] using a binary operator, going right to left */
  def foldr[R](s: S)(r: R)(f: (A, R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of a [[ATraversal_]] using a binary operator, going left to right */
  def foldl[R](s: S)(r: R)(f: (R, A) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** evaluate each  focus of a [[ATraversal_]] from left to right, and ignore the results structure  */
  def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  /** map each focus of a [[ATraversal_]] to an effect, from left to right, and ignore the results */
  def traverse_[F[_], R](s: S)(f: A => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldr[F[Unit]](s)(ev.pure(()))((a, b) => ev.void(f(a)) *> b)

  /** the sum of all foci of a [[ATraversal_]] */
  def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMapNewtype[Additive[A], A](s)(identity)

  /** the product of all foci of a [[ATraversal_]] */
  def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMapNewtype[Multiplicative[A], A](s)(identity)

  /** test whether there is no focus or a predicate holds for all foci of a [[ATraversal_]] */
  def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for all foci of a [[ATraversal_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** return the result of a conjunction of all foci of a [[ATraversal_]], using a [[Heyting]] algebra */
  def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(identity)

  /** return the result of a disjunction of all foci of a [[ATraversal_]], using a [[Heyting]] algebra */
  def or(s: S)(implicit ev: Heyting[A]): A = any[A](s)(identity)

  /** test whether a predicate holds for any focus of a [[ATraversal_]], using a [[Heyting]] algebra */
  def any[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Disj[R], R](s)(f)

  /** test whether a predicate holds for any foci of a [[ATraversal_]] */
  def exists(f: A => Boolean): S => Boolean = any[Boolean](_)(f)

  /** test whether a predicate does not hold for the foci of a [[ATraversal_]] */
  def notExists(f: A => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a [[ATraversal_]] contains a specific focus */
  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether a [[ATraversal_]] does not contain a specific focus */
  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  /** check if the [[ATraversal_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[ATraversal_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of a [[ATraversal_]] */
  def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of a [[ATraversal_]] that satisfies a predicate, if there is any */
  def find(f: A => Boolean): S => Option[A] =
    foldr[Option[A]](_)(None)((a, b) => b.fold(if (f(a)) a.some else None)(Some[A]))

  /** find the first focus of a [[ATraversal_]], if there is any. Synonym for preview */
  def first(s: S): Option[A] = preview(s)

  /** find the last focus of a [[ATraversal_]], if there is any */
  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](s)(_.some)

  /** the minimum of all foci of a [[ATraversal_]], if there is any */
  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of a [[ATraversal_]], if there is any */
  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of a [[ATraversal_]] into an [[Array]] */
  def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  /** synonym to [[viewAll]] */
  def toList(s: S): List[A] = viewAll(s)

  /** collect all the foci of a [[ATraversal_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, List[A]] = ev.inspect(viewAll)

  /** transform an [[ATraversal_]] to a [[Traversal_]] */
  def asTraversal: Traversal_[S, T, A, B] = new Traversal_[S, T, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, A, B] = new Traversing[S, T, A, B] {
        override def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = self.traverse(s)(f)
      }

      ev.wander(traversing)(pab)
    }
  }

  /** compose an [[ATraversal_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](c2fd: C => F[D])(s: S)(implicit ev0: Applicative[F]): F[T] =
        self.traverse(s)(a => ev0.map(c2fd(other.view(a)))(other.set(_)(a)))
    })

  /** compose an [[ATraversal_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): ATraversal_[S, T, C, D] = self compose other.asIso

  /** compose an [[ATraversal_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](c2fd: C => F[D])(s: S)(implicit ev0: Applicative[F]): F[T] =
        self.traverse(s)(a => ev0.map(c2fd(other.view(a)))(other.set(_)(a)))
    })

  /** compose an [[ATraversal_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): ATraversal_[S, T, C, D] = self compose other.asLens

  /** compose an [[ATraversal_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[ATraversal_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[AffineTraversal_]] with an [[ATraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): ATraversal_[S, T, C, D] = new ATraversal_[S, T, C, D] {
    override private[proptics] def apply(bazaar: Bazaar[* => *, C, D, C, D]): Bazaar[* => *, C, D, S, T] =
      new Bazaar[* => *, C, D, S, T] {
        override def runBazaar: RunBazaar[* => *, C, D, S, T] = new RunBazaar[* => *, C, D, S, T] {
          override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] = traverse[F](s)(pafb)
        }
      }

    /** modify each focus of a [[ATraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure  */
    override def traverse[G[_]](s: S)(f: C => G[D])(implicit ev: Applicative[G]): G[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  /** compose an [[ATraversal_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[ATraversal_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[ATraversal_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self.traverse[Id](_)(other(pab))
  }

  /** compose an [[ATraversal_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold

  /** compose an [[ATraversal_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldr[Option[A]](s)(None)((a, op) => f(a, op.getOrElse(a)).some)
}

object ATraversal_ {

  /** create a polymorphic [[ATraversal_]] from RunBazaar encoding */
  private[proptics] def apply[S, T, A, B](runBazaar: RunBazaar[* => *, A, B, S, T]): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
    override private[proptics] def apply(bazaar: Bazaar[Function, A, B, A, B]): Bazaar[Function, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = runBazaar(pafb)(s)
      }
    }

    override def traverse[G[_]](s: S)(f: A => G[B])(implicit ev: Applicative[G]): G[T] = runBazaar(f)(s)
  }

  /** create a polymorphic [[ATraversal_]] from a getter/setter pair */
  def apply[S, T, A, B](get: S => A)(_set: S => B => T): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = traverse(s)(pafb)
      }
    }

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] =
      ev.map(f(get(s)))(_set(s)(_))
  }

  /** create a polymorphic [[ATraversal_]] from a combined getter/setter */
  def traverse[P[_, _], S, T, A, B](to: S => (A, B => T)): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
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

  /** create a polymorphic [[ATraversal_]] from a [[Traverse]] */
  def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): ATraversal_[G[A], G[B], A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, G[A], G[B]] {
      override def apply[F[_]](pafb: A => F[B])(s: G[A])(implicit ev: Applicative[F]): F[G[B]] =
        ev0.traverse(s)(pafb)
    })
}

object ATraversal {

  /** create a momnomorphic [[ATraversal]] from a getter/setter pair */
  def apply[S, A](get: S => A)(set: S => A => S): ATraversal[S, A] = ATraversal_(get)(set)

  /** create a monomorphic [[ATraversal]] from a combined getter/setter */
  def traverse[S, A](to: S => (A, A => S)): ATraversal[S, A] = ATraversal_.traverse(to)

  /** create a monomorphic [[ATraversal]] from a [[Traverse]] */
  def fromTraverse[G[_]: Traverse, A]: ATraversal[G[A], A] = ATraversal_.fromTraverse
}
