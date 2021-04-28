package proptics

import scala.Function.const
import scala.reflect.ClassTag

import cats.data.{Const, State}
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, Order, Traverse}
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}
import spire.std.boolean._

import proptics.data.{Additive, Conj, Disj, Dual, Endo, First, Last, Multiplicative}
import proptics.internal._
import proptics.profunctor.{Traversing, Wander}
import proptics.rank2types.LensLikeWithIndex
import proptics.syntax.function._

/** [[ATraversal_]] is an optic that focuses on zero or more values.
  *
  * [[ATraversal_]] is a [[Traversal_]] with fixed type [[Bazaar]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of a [[ATraversal_]]
  * @tparam T the modified source of a [[ATraversal_]]
  * @tparam A the foci of a [[ATraversal_]]
  * @tparam B the modified foci of a [[ATraversal_]]
  */
abstract class ATraversal_[S, T, A, B] { self =>
  private[proptics] def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T]

  /** synonym to [[fold]] */
  final def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  /** collect all the foci of a [[ATraversal_]] into a [[List]] */
  final def viewAll(s: S): List[A] = foldMap(s)(List(_))

  /** view the first focus of a [[ATraversal_]], if there is any */
  final def preview(s: S): Option[A] = foldMap(s)(a => First(a.some)).runFirst

  /** set the modified foci of a [[ATraversal_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the foci type of a [[Prism_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = s => traverse[Id](s)(f)

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify each focus of an [[ATraversal_]] using a Functor, resulting in a change of type to the full structure */
  def traverse[G[_]](s: S)(f: A => G[B])(implicit ev: Applicative[G]): G[T]

  /** map each focus of an [[ATraversal_] to a Monoid, and combine the results */
  final def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** fold the foci of a [[ATraversal_]] using a Monoid */
  final def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  /** fold the foci of a [[ATraversal_]] using a binary operator, going right to left */
  final def foldRight[R](s: S)(r: R)(f: (A, R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of a [[ATraversal_]] using a binary operator, going left to right */
  final def foldLeft[R](s: S)(r: R)(f: (R, A) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** evaluate each  focus of a [[ATraversal_]] from left to right, and ignore the results structure */
  final def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  /** map each focus of a [[ATraversal_]] to an effect, from left to right, and ignore the results */
  final def traverse_[F[_], R](s: S)(f: A => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldLeft[F[Unit]](s)(ev.pure(()))((b, a) => ev.void(f(a)) *> b)

  /** the sum of all foci of a [[ATraversal_]] */
  final def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMap(s)(Additive.apply).runAdditive

  /** the product of all foci of a [[ATraversal_]] */
  final def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A =
    foldMap(s)(Multiplicative.apply).runMultiplicative

  /** test whether there is no focus or a predicate holds for all foci of a [[ATraversal_]] */
  final def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for all foci of a [[ATraversal_]], using a Heyting algebra */
  final def forall[R: Heyting](s: S)(f: A => R): R = foldMap(s)(Conj[R] _ compose f).runConj

  /** return the result of a conjunction of all foci of a [[ATraversal_]], using a Heyting algebra */
  final def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(identity)

  /** return the result of a disjunction of all foci of a [[ATraversal_]], using a Heyting algebra */
  final def or(s: S)(implicit ev: Heyting[A]): A = any[A](s)(identity)

  /** test whether a predicate holds for any focus of a [[ATraversal_]], using a Heyting algebra */
  final def any[R: Heyting](s: S)(f: A => R): R = foldMap(s)(Disj[R] _ compose f).runDisj

  /** test whether a predicate holds for any foci of a [[ATraversal_]] */
  final def exists(f: A => Boolean): S => Boolean = any[Boolean](_)(f)

  /** test whether a predicate does not hold for the foci of a [[ATraversal_]] */
  final def notExists(f: A => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a [[ATraversal_]] contains a specific focus */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether a [[ATraversal_]] does not contain a specific focus */
  final def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** check if the [[ATraversal_]] does not contain a focus */
  final def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[ATraversal_]] contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of a [[ATraversal_]] */
  final def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of a [[ATraversal_]] that satisfies a predicate, if there is any */
  final def find(f: A => Boolean): S => Option[A] =
    foldRight[Option[A]](_)(None)((a, b) => b.fold(if (f(a)) a.some else None)(Some[A]))

  /** find the first focus of a [[ATraversal_]], if there is any. Synonym for preview */
  final def first(s: S): Option[A] = preview(s)

  /** find the last focus of a [[ATraversal_]], if there is any */
  final def last(s: S): Option[A] = foldMap(s)(a => Last(a.some)).runLast

  /** the minimum of all foci of a [[ATraversal_]], if there is any */
  final def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of a [[ATraversal_]], if there is any */
  final def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of a [[ATraversal_]] into an Array */
  final def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  /** synonym to [[viewAll]] */
  final def toList(s: S): List[A] = viewAll(s)

  /** collect all the foci of a [[ATraversal_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, List[A]] = ev.inspect(viewAll)

  /** convert an [[ATraversal_]] to a Bazaar[* => *, A, B, S, T] */
  final def toBazaar: Bazaar[* => *, A, B, S, T] = self(new Bazaar[* => *, A, B, A, B] {
    override def runBazaar: RunBazaar[* => *, A, B, A, B] = new RunBazaar[* => *, A, B, A, B] {
      override def apply[F[_]](pafb: A => F[B])(s: A)(implicit ev: Applicative[F]): F[B] = pafb(s)
    }
  })

  /** transform an [[ATraversal_]] to a [[Traversal_]] */
  final def asTraversal: Traversal_[S, T, A, B] = new Traversal_[S, T, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, A, B] = new Traversing[S, T, A, B] {
        override def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = self.traverse(s)(f)
      }

      ev.wander(traversing)(pab)
    }
  }

  /** transform an [[ATraversal_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget))
  }

  /** compose a [[ATraversal_]] with a function lifted to a [[Getter_]] */
  final def to[C, D](f: A => C): Fold_[S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose an [[ATraversal_]] with an [[Iso_]] */
  final def compose[C, D](other: Iso_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](c2fd: C => F[D])(s: S)(implicit ev0: Applicative[F]): F[T] =
        self.traverse(s)(a => ev0.map(c2fd(other.view(a)))(other.set(_)(a)))
    })

  /** compose an [[ATraversal_]] with an [[AnIso_]] */
  final def compose[C, D](other: AnIso_[A, B, C, D]): ATraversal_[S, T, C, D] = self compose other.asIso

  /** compose an [[ATraversal_]] with a [[Lens_]] */
  final def compose[C, D](other: Lens_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](c2fd: C => F[D])(s: S)(implicit ev0: Applicative[F]): F[T] =
        self.traverse(s)(a => ev0.map(c2fd(other.view(a)))(other.set(_)(a)))
    })

  /** compose an [[ATraversal_]] with an [[ALens_]] */
  final def compose[C, D](other: ALens_[A, B, C, D]): ATraversal_[S, T, C, D] = self compose other.asLens

  /** compose an [[ATraversal_]] with a [[Prism_]] */
  final def compose[C, D](other: Prism_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[ATraversal_]] with an [[APrism_]] */
  final def compose[C, D](other: APrism_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[ATraversal_]] with an [[AffineTraversal_]] */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): ATraversal_[S, T, C, D] = new ATraversal_[S, T, C, D] {
    override private[proptics] def apply(bazaar: Bazaar[* => *, C, D, C, D]): Bazaar[* => *, C, D, S, T] =
      new Bazaar[* => *, C, D, S, T] {
        override def runBazaar: RunBazaar[* => *, C, D, S, T] = new RunBazaar[* => *, C, D, S, T] {
          override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] = traverse[F](s)(pafb)
        }
      }

    /** modify each focus of a [[ATraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
    override def traverse[G[_]](s: S)(f: C => G[D])(implicit ev: Applicative[G]): G[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  /** compose an [[ATraversal_]] with an [[AnAffineTraversal_]] */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): ATraversal_[S, T, C, D] = new ATraversal_[S, T, C, D] {
    override private[proptics] def apply(bazaar: Bazaar[* => *, C, D, C, D]): Bazaar[* => *, C, D, S, T] =
      new Bazaar[* => *, C, D, S, T] {
        override def runBazaar: RunBazaar[* => *, C, D, S, T] = new RunBazaar[* => *, C, D, S, T] {
          override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] = traverse[F](s)(pafb)
        }
      }

    /** modify each focus of a [[ATraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
    override def traverse[G[_]](s: S)(f: C => G[D])(implicit ev: Applicative[G]): G[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  /** compose an [[ATraversal_]] with a [[Traversal_]] */
  final def compose[C, D](other: Traversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[ATraversal_]] with an [[ATraversal_]] */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[ATraversal_]] with a [[Setter_]] */
  final def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self.traverse[Id](_)(other(pab))
  }

  /** compose an [[ATraversal_]] with a [[Getter_]] */
  final def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold

  /** compose an [[ATraversal_]] with a [[Fold_]] */
  final def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose an [[ATraversal_]] with an [[IndexedLens_]] */
  final def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose an [[ATraversal_]] with an [[AnIndexedLens_]] */
  final def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose an [[ATraversal_]] with an [[IndexedTraversal_]] */
  final def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose a [[Traversal_]] with an [[IndexedSetter_]] */
  final def compose[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose a [[Traversal_]] with an [[IndexedGetter_]] */
  final def compose[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose an [[ATraversal_]] with an [[IndexedFold_]] */
  final def compose[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  private def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldRight[Option[A]](s)(None)((a, op) => f(a, op.getOrElse(a)).some)
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
  final def apply[S, T, A, B](get: S => A)(_set: S => B => T): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = traverse(s)(pafb)
      }
    }

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] =
      ev.map(f(get(s)))(_set(s)(_))
  }

  /** create a polymorphic [[ATraversal_]] from a combined getter/setter */
  final def traverse[P[_, _], S, T, A, B](combined: S => (A, B => T)): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = traverse(s)(pafb)
      }
    }

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev1: Applicative[F]): F[T] = {
      val (a, b2t) = combined(s)

      ev1.map(f(a))(b2t)
    }
  }

  /** create a polymorphic [[ATraversal_]] from a [[Traverse]] */
  final def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): ATraversal_[G[A], G[B], A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, G[A], G[B]] {
      override def apply[F[_]](pafb: A => F[B])(s: G[A])(implicit ev: Applicative[F]): F[G[B]] =
        ev0.traverse(s)(pafb)
    })

  /** create a polymorphic [[ATraversal_]] from [[Bazaar]] */
  final def fromBazaar[S, T, A, B](bazaar: Bazaar[* => *, A, B, S, T]): ATraversal_[S, T, A, B] = ATraversal_[S, T, A, B](bazaar.runBazaar)

  /** polymorphic identity of an [[ATraversal_]] */
  final def id[S, T]: ATraversal_[S, T, S, T] = ATraversal_(identity[S] _)(const(identity[T]))
}

object ATraversal {
  /** create a monomorphic [[ATraversal]] from a getter/setter pair */
  final def apply[S, A](get: S => A)(set: S => A => S): ATraversal[S, A] = ATraversal_(get)(set)

  /** create a monomorphic [[ATraversal]] from a combined getter/setter */
  final def traverse[S, A](to: S => (A, A => S)): ATraversal[S, A] = ATraversal_.traverse(to)

  /** create a monomorphic [[ATraversal]] from a [[Traverse]] */
  final def fromTraverse[G[_]: Traverse, A]: ATraversal[G[A], A] = ATraversal_.fromTraverse

  /** create a monomorphic [[ATraversal]] from a [[Bazaar]] */
  final def fromBazaar[S, A](bazaar: Bazaar[* => *, A, A, S, S]): ATraversal[S, A] = ATraversal_.fromBazaar(bazaar)

  /** monomorphic identity of an [[ATraversal]] */
  final def id[S]: ATraversal[S, S] = ATraversal_.id[S, S]
}
