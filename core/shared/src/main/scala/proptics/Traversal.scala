package proptics

import scala.Function.const

import cats.data.{Nested, State}
import cats.syntax.apply._
import cats.syntax.bitraverse._
import cats.{Applicative, Bitraverse, Monoid, Traverse}

import proptics.Lens_.liftOptic
import proptics.Traversal_.wander
import proptics.internal._
import proptics.profunctor.Corepresentable.Aux
import proptics.profunctor.Wander._
import proptics.profunctor.{Star, Traversing, Wander}
import proptics.rank2types.{LensLike, LensLikeWithIndex, Rank2TypeTraversalLike}
import proptics.syntax.star._
import proptics.syntax.traversal._

/** A [[Traversal_]] is an optic that focuses on zero or more values
  *
  * @tparam S
  *   the source of a [[Traversal_]]
  * @tparam T
  *   the modified source of a [[Traversal_]]
  * @tparam A
  *   the foci of a [[Traversal_]]
  * @tparam B
  *   the modified foci of a [[Traversal_]]
  */
abstract class Traversal_[S, T, A, B] extends Traversal1[S, T, A, B] { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]

  /** modify the foci type of a [[Traversal_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = self(f)

  /** modify each focus of a [[Traversal_]] using a Functor, resulting in a change of type to the full structure */
  final override def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  /** convert a [[Traversal_]] to a [[proptics.internal.Bazaar]] */
  final def toBazaar: Bazaar[* => *, A, B, S, T] = self(new Bazaar[* => *, A, B, A, B] {
    override def runBazaar: RunBazaar[* => *, A, B, A, B] = new RunBazaar[* => *, A, B, A, B] {
      override def apply[F[_]](pafb: A => F[B])(s: A)(implicit ev: Applicative[F]): F[B] = pafb(s)
    }
  })

  /** transform a [[Traversal_]] to an [[ATraversal_]] */
  final def asATraversal: ATraversal_[S, T, A, B] =
    ATraversal_[S, T, A, B](new RunBazaar[* => *, A, B, S, T] {
      override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = self.traverse(s)(pafb)
    })

  /** convert a [[Traversal_]] to an [[IndexedTraversal_]] by using the integer positions as indices */
  final def zipWithIndex(implicit ev0: Applicative[State[Int, *]]): IndexedTraversal_[Int, S, T, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[Int, S, T, A, B] {
      override def apply[F[_]](f: ((A, Int)) => F[B])(implicit ev1: Applicative[F]): S => F[T] = s => {
        val state: State[Int, Unit] = State.apply[Int, Unit](i => (i, ()))
        val starNested: Star[Nested[State[Int, *], F, *], A, B] = Star { a =>
          val composed = (state.get, ev0.pure(a)).mapN((i, a) => f((a, i))) <* state.modify(_ + 1)

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
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget))
  }

  /** convert a [[Traversal]] into a [[Lens]] over a list of the [[Traversal]]'s foci */
  final def unsafePartsOf(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]], ev1: Aux[* => *, State[List[B], *]]): Lens_[S, T, List[A], List[B]] =
    Bazaar.unsafePartsOf(self.toBazaar)

  /** compose this [[Traversal_]] with a function lifted to a [[Getter_]], having this [[Traversal_]] applied first */
  final def to[C, D](f: A => C): Fold_[S, T, C, D] = andThen(Getter_[A, B, C, D](f))

  /** compose this [[Traversal_]] with an [[Iso_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose this [[Traversal_]] with an [[Iso_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): Traversal_[C, D, A, B] = new Traversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[C, D] = other(self(pab))
  }

  /** compose this [[Traversal_]] with an [[AnIso_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): Traversal_[S, T, C, D] =
    wander(new LensLike[S, T, C, D] {
      override def apply[F[_]](f: C => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[Traversal_]] with an [[AnIso_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): Traversal_[C, D, A, B] =
    wander(new LensLike[C, D, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF(f))
    })

  /** compose this [[Traversal_]] with a [[Lens_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose this [[Traversal_]] with a [[Lens_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): Traversal_[C, D, A, B] = new Traversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[C, D] = other(self(pab))
  }

  /** compose this [[Traversal_]] with an [[ALens_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): Traversal_[S, T, C, D] =
    wander(new LensLike[S, T, C, D] {
      override def apply[F[_]](f: C => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose this [[Traversal_]] with an [[ALens_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): Traversal_[C, D, A, B] =
    wander(new LensLike[C, D, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF(f))
    })

  /** compose this [[Traversal_]] with a [[Prism_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose this [[Traversal_]] with a [[Prism_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): Traversal_[C, D, A, B] = new Traversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[C, D] = other(self(pab))
  }

  /** compose this [[Traversal_]] with an [[APrism_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): Traversal_[S, T, C, D] =
    wander(new LensLike[S, T, C, D] {
      override def apply[F[_]](f: C => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose this [[Traversal_]] with an [[APrism_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): Traversal_[C, D, A, B] =
    wander(new LensLike[C, D, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF(f))
    })

  /** compose this [[Traversal_]] with an [[AffineTraversal_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose this [[Traversal_]] with an [[AffineTraversal_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): Traversal_[C, D, A, B] = new Traversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[C, D] = other(self(pab))
  }

  /** compose this [[Traversal_]] with an [[AnAffineTraversal_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): Traversal_[S, T, C, D] =
    wander(new LensLike[S, T, C, D] {
      override def apply[F[_]](f: C => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.traverse(_)(f))
    })

  /** compose this [[Traversal_]] with an [[AnAffineTraversal_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): Traversal_[C, D, A, B] =
    wander(new LensLike[C, D, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF(f))
    })

  /** compose this [[Traversal_]] with a [[Traversal_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose this [[Traversal_]] with a [[Traversal_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): Traversal_[C, D, A, B] = new Traversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[C, D] = other(self(pab))
  }

  /** compose this [[Traversal_]] with an [[ATraversal_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose this [[Traversal_]] with an [[ATraversal_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[Traversal_]] with a [[Setter_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose this [[Traversal_]] with a [[Setter_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: Setter_[C, D, S, T]): Setter_[C, D, A, B] = new Setter_[C, D, A, B] {
    override private[proptics] def apply(pab: A => B): C => D = other(self(pab))
  }

  /** compose this [[Traversal_]] with a [[Getter_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose this [[Traversal_]] with a [[Getter_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Traversal_]] with a [[Fold_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Traversal_]] with a [[Fold_]], having this [[Traversal_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] = other(self(forget))
  }

  /** compose this [[Traversal_]] with an [[IndexedLens_]], having this [[Traversal_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose this [[Traversal_]] with an [[IndexedLens_]], having this [[Traversal_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[Traversal_]] with an [[AnIndexedLens_]], having this [[Traversal_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose this [[Traversal_]] with an [[AnIndexedLens_]], having this [[Traversal_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[Traversal_]] with an [[IndexedTraversal_]], having this [[Traversal_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose this [[Traversal_]] with an [[IndexedTraversal_]], having this [[Traversal_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[Traversal_]] with an [[IndexedSetter_]], having this [[Traversal_]] applied first */
  final def andThen[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose this [[Traversal_]] with an [[IndexedSetter_]], having this [[Traversal_]] applied last */
  final def compose[I, C, D](other: IndexedSetter_[I, C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[Function, I, A, B]): C => D =
      other.over { case (s, i) => self.over(a => indexed.runIndex((a, i)))(s) }
  }

  /** compose this [[Traversal_]] with an [[IndexedGetter_]], having this [[Traversal_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[Traversal_]] with an [[IndexedGetter_]], having this [[Traversal_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[Traversal_]] with an [[IndexedFold_]], having this [[Traversal_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[Traversal_]] with an [[IndexedFold_]], having this [[Traversal_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => self.foldMap(s)(a => indexed.runIndex.runForget((a, i))) })
  }
}

object Traversal_ {
  /** create a polymorphic [[Traversal_]] from Rank2TypeTraversalLike encoding */
  private[proptics] def apply[S, T, A, B](lensLikeTraversal: Rank2TypeTraversalLike[S, T, A, B]): Traversal_[S, T, A, B] = new Traversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Wander[P]): P[S, T] = lensLikeTraversal(pab)
  }

  /** create a polymorphic [[Traversal_]] from a getter/setter pair */
  final def apply[S, T, A, B](get: S => A)(_set: S => B => T): Traversal_[S, T, A, B] = new Traversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, A, B] {
        override def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = ev.map(f(get(s)))(_set(s))
      }

      ev.wander(traversing)(pab)
    }
  }

  /** create a polymorphic [[Traversal_]] from a combined getter/setter */
  final def traversal[S, T, A, B](to: S => (A, B => T)): Traversal_[S, T, A, B] = Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = liftOptic(to)(ev)(pab)
  })

  /** traverse elements of a [[Traversal_]] that satisfy a predicate */
  final def filter[A](predicate: A => Boolean): Traversal_[A, A, A, A] =
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
  final def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): Traversal_[G[A], G[B], A, B] = Traversal_(new Rank2TypeTraversalLike[G[A], G[B], A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev1: Wander[P]): P[G[A], G[B]] = {
      val traversing = new Traversing[G[A], G[B], A, B] {
        override def apply[F[_]](f: A => F[B])(s: G[A])(implicit ev2: Applicative[F]): F[G[B]] =
          ev0.traverse[F, A, B](s)(f)
      }

      ev1.wander(traversing)(pab)
    }
  })

  /** create a polymorphic [[Traversal_]] from [[proptics.internal.Bazaar]] */
  final def fromBazaar[S, T, A, B](bazaar: Bazaar[* => *, A, B, S, T]): Traversal_[S, T, A, B] =
    Traversal_[S, T, A, B](new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
        val traversing = new Traversing[S, T, A, B] {
          override def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = bazaar.runBazaar(f)(s)
        }

        ev.wander(traversing)(pab)
      }
    })

  /** traverse both parts of a [[cats.Bitraverse]] with matching types */
  final def both[G[_, _]: Bitraverse, A, B]: Traversal_[G[A, A], G[B, B], A, B] =
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
  final def wander[S, T, A, B](lensLike: LensLike[S, T, A, B]): Traversal_[S, T, A, B] =
    Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Wander[P]): P[S, T] = {
        def traversing: Traversing[S, T, A, B] = new Traversing[S, T, A, B] {
          override def apply[F[_]](f: A => F[B])(s: S)(implicit ev1: Applicative[F]): F[T] = lensLike[F](f)(ev1)(s)
        }

        ev0.wander(traversing)(pab)
      }
    })

  /** polymorphic identity of a [[Traversal_]] */
  final def id[S, T]: Traversal_[S, T, S, T] = Traversal_(identity[S] _)(const(identity[T]))

  /** convert a [[Traversal]] into a [[Lens]] over a list of the [[Traversal]]'s foci */
  final def unsafePartsOf[S, T, A, B](
      traversal: Traversal_[S, T, A, B])(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]], ev2: Aux[* => *, State[List[B], *]]): Lens_[S, T, List[A], List[B]] =
    traversal.unsafePartsOf
}

object Traversal {
  /** create a monomorphic [[Traversal]] from Rank2TypeTraversalLike encoding */
  final def fromTraversal[S, A](lensLikeTraversal: Rank2TypeTraversalLike[S, S, A, A]): Traversal[S, A] =
    Traversal_[S, S, A, A](lensLikeTraversal)

  /** create a monomorphic [[Traversal]] from a getter/setter pair */
  final def apply[S, A](get: S => A)(set: S => A => S): Traversal[S, A] = Traversal_(get)(set)

  /** create a monomorphic [[Traversal]] from a combined getter/setter */
  final def traversal[S, A](to: S => (A, A => S)): Traversal[S, A] = Traversal_.traversal(to)

  /** traverse elements of a [[Traversal]], that satisfy a predicate */
  final def filter[A](predicate: A => Boolean): Traversal[A, A] = Traversal_.filter(predicate)

  /** traverse elements of a [[Traversal]], by taking the first element of a [[Fold]] and using it as a filter */
  final def filter[A, B](fold: Fold[A, B]): Traversal[A, A] =
    Traversal_[A, A, A, A](new Rank2TypeTraversalLike[A, A, A, A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev0: Wander[P]): P[A, A] = {
        val traversing = new Traversing[A, A, A, A] {
          override def apply[F[_]](f: A => F[A])(s: A)(implicit ev1: Applicative[F]): F[A] =
            fold.preview(s).fold(ev1.pure(s))(const(f(s)))
        }

        ev0.wander(traversing)(pab)
      }
    })

  /** create a monomorphic [[Traversal]] from a [[cats.Traverse]] */
  final def fromTraverse[F[_]: Traverse, A]: Traversal[F[A], A] = Traversal_.fromTraverse

  /** create a monomorphic [[Traversal]] from a [[proptics.internal.Bazaar]] */
  final def fromBazaar[S, A](bazaar: Bazaar[* => *, A, A, S, S]): Traversal[S, A] = Traversal_.fromBazaar(bazaar)

  /** traverse both parts of a [[cats.Bitraverse]] with matching types */
  final def both[G[_, _]: Bitraverse, A]: Traversal[G[A, A], A] = Traversal_.both[G, A, A]

  /** create a monomorphic [[Traversal]] from a rank 2 type traversal function */
  final def wander[S, A](lensLike: LensLike[S, S, A, A]): Traversal[S, A] =
    Traversal_.wander[S, S, A, A](lensLike)

  /** monomorphic identity of a [[Traversal]] */
  final def id[S]: Traversal[S, S] = Traversal_.id[S, S]

  /** create a monomorphic [[Traversal]] that narrows the focus to a single element */
  final def elementAt[F[_]: Traverse, A](i: Int): Traversal[F[A], A] = Traversal.fromTraverse[F, A].elementAt(i)

  /** create a monomorphic [[Traversal]] that selects the first n elements of a Traverse */
  final def take[F[_]: Traverse, A](i: Int): Traversal[F[A], A] = Traversal.fromTraverse[F, A].take(i)

  /** create a monomorphic [[Traversal]] that selects all elements of a Traverse except the first n ones */
  final def drop[F[_]: Traverse, A](i: Int): Traversal[F[A], A] = Traversal.fromTraverse[F, A].drop(i)

  /** create a monomorphic [[Traversal]] that takes the longest prefix of elements of a Traverse that satisfy a predicate */
  final def takeWhile[G[_]: Traverse, A](predicate: A => Boolean): Traversal[G[A], A] =
    Traversal.fromTraverse[G, A].takeWhile(predicate)

  /** create a monomorphic [[Traversal]] that drop longest prefix of elements of a Traverse that satisfy a predicate */
  final def dropWhile[G[_]: Traverse, A](predicate: A => Boolean): Traversal[G[A], A] =
    Traversal.fromTraverse[G, A].dropWhile(predicate)

  /** convert a [[Traversal]] into a [[Lens]] over a list of the [[Traversal]]'s foci */
  final def partsOf[S, T, A](traversal: Traversal_[S, T, A, A])(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]]): Lens_[S, T, List[A], List[A]] =
    Bazaar.partsOf(traversal.toBazaar)
}

object Traversal2 {
  /** create a polymorphic [[Traversal_]] using two view functions that accept the same structure, and a setter function, and simultaneously focus on two distinct parts of it
    */
  def apply[S, T, A, B](view1: S => A, view2: S => A)(set: (B, B, S) => T): Traversal_[S, T, A, B] =
    Traversal_.wander(new LensLike[S, T, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] = s => ev.map2(f(view1(s)), f(view2(s)))(set(_, _, s))
    })
}

object Traversal3 {
  /** create a polymorphic [[Traversal_]] using three view functions that accept the same structure, and a setter function, and simultaneously focus on three distinct parts of it
    */
  def apply[S, T, A, B](view1: S => A, view2: S => A, view3: S => A)(set: (B, B, B, S) => T): Traversal_[S, T, A, B] =
    Traversal_.wander(new LensLike[S, T, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] = s => ev.map3(f(view1(s)), f(view2(s)), f(view3(s)))(set(_, _, _, s))
    })
}

object Traversal4 {
  /** create a polymorphic [[Traversal_]] using four view functions that accept the same structure, and a setter function, and simultaneously focus on four distinct parts of it
    */
  def apply[S, T, A, B](view1: S => A, view2: S => A, view3: S => A, view4: S => A)(set: (B, B, B, B, S) => T): Traversal_[S, T, A, B] =
    Traversal_.wander(new LensLike[S, T, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] = s => ev.map4(f(view1(s)), f(view2(s)), f(view3(s)), f(view4(s)))(set(_, _, _, _, s))
    })
}

object Traversal5 {
  /** create a polymorphic [[Traversal_]] using five view functions that accept the same structure, and a setter function, and simultaneously focus on five distinct parts of it
    */
  def apply[S, T, A, B](view1: S => A, view2: S => A, view3: S => A, view4: S => A, view5: S => A)(set: (B, B, B, B, B, S) => T): Traversal_[S, T, A, B] =
    Traversal_.wander(new LensLike[S, T, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] =
        s => ev.map5(f(view1(s)), f(view2(s)), f(view3(s)), f(view4(s)), f(view5(s)))(set(_, _, _, _, _, s))
    })
}
