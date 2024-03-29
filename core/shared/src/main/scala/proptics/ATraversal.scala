package proptics

import scala.Function.const

import cats.data.State
import cats.syntax.bitraverse._
import cats.{Applicative, Bitraverse, Id, Monoid, Traverse}

import proptics.internal._
import proptics.profunctor.Corepresentable.Aux
import proptics.profunctor.{Traversing, Wander}
import proptics.rank2types.LensLikeWithIndex
import proptics.syntax.aTraversal._

/** [[ATraversal_]] is an optic that focuses on zero or more values.
  *
  * [[ATraversal_]] is a [[Traversal_]] with fixed type [[proptics.internal.Bazaar]] [[cats.arrow.Profunctor]]
  *
  * @tparam S
  *   the source of a [[ATraversal_]]
  * @tparam T
  *   the modified source of a [[ATraversal_]]
  * @tparam A
  *   the foci of a [[ATraversal_]]
  * @tparam B
  *   the modified foci of a [[ATraversal_]]
  */
abstract class ATraversal_[S, T, A, B] extends Traversal1[S, T, A, B] { self =>
  private[proptics] def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T]

  /** modify the foci type of a [[Prism_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = s => traverse[Id](s)(f)

  /** modify each focus of an [[ATraversal_]] using a Functor, resulting in a change of type to the full structure */
  def traverse[G[_]](s: S)(f: A => G[B])(implicit ev: Applicative[G]): G[T]

  /** convert an [[ATraversal_]] to a [[proptics.internal.Bazaar]] */
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

  /** convert an [[ATraversal_]] to an [[IndexedTraversal_]] by using the integer positions as indices */
  final def asIndexableTraversal(implicit ev0: Applicative[State[Int, *]]): IndexedTraversal_[Int, S, T, A, B] =
    self.asTraversal.zipWithIndex

  /** transform an [[ATraversal_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget))
  }

  /** convert a [[ATraversal]] into a [[Lens]] over a list of the [[ATraversal]]'s foci */
  final def unsafePartsOf(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]], ev1: Aux[* => *, State[List[B], *]]): Lens_[S, T, List[A], List[B]] =
    Bazaar.unsafePartsOf(self.toBazaar)

  /** compose this [[ATraversal_]] with a function lifted to a [[Getter_]], having this [[ATraversal_]] applied first */
  final def focus[C, D](f: A => C): Fold_[S, T, C, D] = andThen(Getter_[A, B, C, D](f))

  /** compose this [[ATraversal_]] with an [[Iso_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](c2fd: C => F[D])(s: S)(implicit ev0: Applicative[F]): F[T] =
        self.traverse(s)(a => ev0.map(c2fd(other.view(a)))(other.set(_)(a)))
    })

  /** compose this [[ATraversal_]] with an [[Iso_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[AnIso_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](c2fd: C => F[D])(s: S)(implicit ev0: Applicative[F]): F[T] =
        self.traverse(s)(a => ev0.map(c2fd(other.view(a)))(other.set(_)(a)))
    })

  /** compose this [[ATraversal_]] with an [[AnIso_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with a [[Lens_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](c2fd: C => F[D])(s: S)(implicit ev0: Applicative[F]): F[T] =
        self.traverse(s)(a => ev0.map(c2fd(other.view(a)))(other.set(_)(a)))
    })

  /** compose this [[ATraversal_]] with a [[Lens_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[ALens_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](c2fd: C => F[D])(s: S)(implicit ev0: Applicative[F]): F[T] =
        self.traverse(s)(a => ev0.map(c2fd(other.view(a)))(other.set(_)(a)))
    })

  /** compose this [[ATraversal_]] with an [[ALens_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with a [[Prism_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[ATraversal_]] with a [[Prism_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[APrism_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[APrism_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[AffineTraversal_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[AffineTraversal_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[AnAffineTraversal_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[AnAffineTraversal_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with a [[Traversal_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[ATraversal_]] with a [[Traversal_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[ATraversal_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[ATraversal_]] with an [[ATraversal_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[ATraversal_]] with a [[Setter_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self.over(other(pab))
  }

  /** compose this [[ATraversal_]] with a [[Setter_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: Setter_[C, D, S, T]): Setter_[C, D, A, B] = new Setter_[C, D, A, B] {
    override private[proptics] def apply(pab: A => B): C => D = other(self.over(pab))
  }

  /** compose this [[ATraversal_]] with a [[Getter_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose this [[ATraversal_]] with a [[Getter_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[ATraversal_]] with a [[Fold_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose this [[ATraversal_]] with a [[Fold_]], having this [[ATraversal_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[ATraversal_]] with an [[IndexedLens_]], having this [[ATraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[ATraversal_]] with an [[IndexedLens_]], having this [[ATraversal_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[ATraversal_]] with an [[AnIndexedLens_]], having this [[ATraversal_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[ATraversal_]] with an [[AnIndexedLens_]], having this [[ATraversal_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[ATraversal_]] with an [[IndexedTraversal_]], having this [[ATraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[ATraversal_]] with an [[IndexedTraversal_]], having this [[ATraversal_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[Traversal_]] with an [[IndexedSetter_]], having this [[ATraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose this [[Traversal_]] with an [[IndexedSetter_]], having this [[ATraversal_]] applied last */
  final def compose[I, C, D](other: IndexedSetter_[I, C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over { case (s, i) => self.over(a => indexed.runIndex((a, i)))(s) }
  }

  /** compose this [[Traversal_]] with an [[IndexedGetter_]], having this [[ATraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[Traversal_]] with an [[IndexedGetter_]], having this [[ATraversal_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[ATraversal_]] with an [[IndexedFold_]], having this [[ATraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[ATraversal_]] with an [[IndexedFold_]], having this [[ATraversal_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => self.foldMap(s)(a => indexed.runIndex.runForget((a, i))) })
  }
}

object ATraversal_ {
  /** create a polymorphic [[ATraversal_]] from RunBazaar encoding */
  private[proptics] def apply[S, T, A, B](runBazaar0: RunBazaar[* => *, A, B, S, T]): ATraversal_[S, T, A, B] = new ATraversal_[S, T, A, B] {
    override private[proptics] def apply(bazaar: Bazaar[Function, A, B, A, B]): Bazaar[Function, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = runBazaar0(pafb)(s)
      }
    }

    override def traverse[G[_]](s: S)(f: A => G[B])(implicit ev: Applicative[G]): G[T] = runBazaar0(f)(s)
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

  /** traverse elements of a [[ATraversal_]] that satisfy a predicate */
  final def filter[A](predicate: A => Boolean): ATraversal_[A, A, A, A] =
    ATraversal_[A, A, A, A](new RunBazaar[* => *, A, A, A, A] {
      override def apply[F[_]](pafb: A => F[A])(s: A)(implicit ev: Applicative[F]): F[A] =
        if (predicate(s)) pafb(s) else ev.pure(s)
    })

  /** create a polymorphic [[ATraversal_]] from a [[cats.Traverse]] */
  final def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): ATraversal_[G[A], G[B], A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, G[A], G[B]] {
      override def apply[F[_]](pafb: A => F[B])(s: G[A])(implicit ev: Applicative[F]): F[G[B]] =
        ev0.traverse(s)(pafb)
    })

  /** create a polymorphic [[ATraversal_]] from [[proptics.internal.Bazaar]] */
  final def fromBazaar[S, T, A, B](bazaar: Bazaar[* => *, A, B, S, T]): ATraversal_[S, T, A, B] = ATraversal_[S, T, A, B](bazaar.runBazaar)

  /** traverse both parts of a [[cats.Bitraverse]] with matching types */
  final def both[G[_, _]: Bitraverse, A, B]: ATraversal_[G[A, A], G[B, B], A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, G[A, A], G[B, B]] {
      override def apply[F[_]](pafb: A => F[B])(s: G[A, A])(implicit ev: Applicative[F]): F[G[B, B]] =
        s.bitraverse(pafb, pafb)
    })

  /** polymorphic identity of an [[ATraversal_]] */
  final def id[S, T]: ATraversal_[S, T, S, T] = ATraversal_(identity[S] _)(const(identity[T]))

  /** convert a [[Traversal]] into a [[Lens]] over a list of the [[Traversal]]'s foci */
  final def unsafePartsOf[S, T, A, B](
      aTraversal: ATraversal_[S, T, A, B])(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]], ev2: Aux[* => *, State[List[B], *]]): Lens_[S, T, List[A], List[B]] =
    aTraversal.unsafePartsOf
}

object ATraversal {
  /** create a monomorphic [[ATraversal]] from a getter/setter pair */
  final def apply[S, A](get: S => A)(set: S => A => S): ATraversal[S, A] = ATraversal_(get)(set)

  /** create a monomorphic [[ATraversal]] from a combined getter/setter */
  final def traverse[S, A](to: S => (A, A => S)): ATraversal[S, A] = ATraversal_.traverse(to)

  /** traverse elements of an [[ATraversal]], that satisfy a predicate */
  final def filter[A](predicate: A => Boolean): ATraversal[A, A] = ATraversal_.filter(predicate)

  /** traverse elements of an [[ATraversal]], by taking the first element of a [[Fold]] and using it as a filter */
  final def filter[A, B](fold: Fold[A, B]): ATraversal[A, A] =
    ATraversal_[A, A, A, A](new RunBazaar[* => *, A, A, A, A] {
      override def apply[F[_]](pafb: A => F[A])(s: A)(implicit ev: Applicative[F]): F[A] =
        fold.preview(s).fold(ev.pure(s))(const(pafb(s)))
    })

  /** create a monomorphic [[ATraversal]] from a [[cats.Traverse]] */
  final def fromTraverse[G[_]: Traverse, A]: ATraversal[G[A], A] = ATraversal_.fromTraverse

  /** traverse both parts of a [[cats.Bitraverse]] with matching types */
  final def both[G[_, _]: Bitraverse, A]: ATraversal[G[A, A], A] = ATraversal_.both[G, A, A]

  /** create a monomorphic [[ATraversal]] from a [[proptics.internal.Bazaar]] */
  final def fromBazaar[S, A](bazaar: Bazaar[* => *, A, A, S, S]): ATraversal[S, A] = ATraversal_.fromBazaar(bazaar)

  /** monomorphic identity of an [[ATraversal]] */
  final def id[S]: ATraversal[S, S] = ATraversal_.id[S, S]

  /** create a monomorphic [[ATraversal]] that narrows the focus to a single element */
  final def single[F[_]: Traverse, A](i: Int): ATraversal[F[A], A] = ATraversal.fromTraverse[F, A].single(i)

  /** create a monomorphic [[ATraversal]] that selects the first n elements of a Traverse */
  final def take[F[_]: Traverse, A](i: Int): ATraversal[F[A], A] = ATraversal.fromTraverse[F, A].take(i)

  /** create a monomorphic [[ATraversal]] that selects all elements of a Traverse except the first n ones */
  final def drop[F[_]: Traverse, A](i: Int): ATraversal[F[A], A] = ATraversal.fromTraverse[F, A].drop(i)

  /** create a monomorphic [[ATraversal]] that takes the longest prefix of elements of a Traverse that satisfy a predicate */
  final def takeWhile[G[_]: Traverse, A](predicate: A => Boolean): ATraversal[G[A], A] =
    ATraversal.fromTraverse[G, A].takeWhile(predicate)

  /** create a monomorphic [[ATraversal]] that drop longest prefix of elements of a Traverse that satisfy a predicate */
  final def dropWhile[G[_]: Traverse, A](predicate: A => Boolean): ATraversal[G[A], A] =
    ATraversal.fromTraverse[G, A].dropWhile(predicate)

  /** convert an [[ATraversal]] into a [[Lens]] over a list of the [[ATraversal]]'s foci */
  final def partsOf[S, T, A](traversal: ATraversal_[S, T, A, A])(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]]): Lens_[S, T, List[A], List[A]] =
    Bazaar.partsOf(traversal.toBazaar)
}
