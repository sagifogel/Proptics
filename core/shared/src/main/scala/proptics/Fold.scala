package proptics

import scala.Function.const

import cats.syntax.bifoldable._
import cats.syntax.monoid._
import cats.{Bifoldable, Foldable, Monoid}

import proptics.internal.{Fold1, Forget, Indexed}
import proptics.rank2types.{Rank2TypeFoldLike, Rank2TypeIndexedFoldLike}
import proptics.syntax.fold._

/** A [[Fold_]] is a generalization of something Foldable. It describes how to retrieve multiple values.
  *
  * A [[Fold_]] is similar to a [[Traversal_]], but it cannot modify its foci.
  *
  * A [[Fold_]] is an Optic with fixed type [[proptics.internal.Forget]] [[cats.arrow.Profunctor]]
  *
  * @tparam S
  *   the source of a [[Fold_]]
  * @tparam T
  *   the modified source of a [[Fold_]]
  * @tparam A
  *   the foci of a [[Fold_]]
  * @tparam B
  *   the modified foci of a [[Fold_]]
  */
abstract class Fold_[S, T, A, B] extends Fold1[S, A] { self =>
  private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T]

  /** synonym for [[fold]] */
  final override def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  /** map each focus of a [[Fold_]] to a [[cats.Monoid]], and combine the results */
  override def foldMap[R: Monoid](s: S)(f: A => R): R = self(Forget(f)).runForget(s)

  /** convert a [[Fold_]] to an [[IndexedFold_]] by using the integer positions as indices */
  final def asIndexableFold: IndexedFold_[Int, S, T, A, B] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[Int, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], Int, A, B])(implicit ev1: Monoid[R]): Forget[R, S, T] = {
        val runForget: ((A, Int)) => R = indexed.runIndex.runForget
        Forget(self.foldLeft(_)((0, ev1.empty)) { case ((i, r), a) => (i + 1, r |+| runForget((a, i))) }._2)
      }
    })

  /** compose this [[Fold_]] with a function lifted to a [[Getter_]], having this [[Fold_]] applied first */
  final def focus[C, D](f: A => C): Fold_[S, T, C, D] = andThen(Getter_[A, B, C, D](f))

  /** compose this [[Fold_]] with an [[Iso_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      self(other(forget)(Forget.profunctorForget))
  }

  /** compose this [[Fold_]] with an [[Iso_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with an [[AnIso_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[AnIso_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with a [[Lens_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[Lens_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with an [[ALens_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[ALens_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with a [[Prism_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[Prism_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.preview(_).fold(Monoid[R].empty)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[APrism_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.viewOrModify(_).fold(const(Monoid[R].empty), forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[APrism_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.preview(_).fold(Monoid[R].empty)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[AffineTraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[AffineTraversal_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.preview(_).fold(Monoid[R].empty)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[AnAffineTraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.viewOrModify(_).fold(const(Monoid.empty[R]), forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[AnAffineTraversal_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.preview(_).fold(Monoid[R].empty)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[Traversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[Traversal_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[ATraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[ATraversal_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[Getter_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose this [[Fold_]] with a [[Getter_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with a [[Fold_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[Fold_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[IndexedLens_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[IndexedLens_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[Fold_]] with an [[AnIndexedLens_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[AnIndexedLens_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[Fold_]] with an [[IndexedTraversal_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[Fold_]] with an [[IndexedTraversal_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => self.foldMap(s)(a => indexed.runIndex.runForget((a, i))) })
  }

  /** compose this [[Fold_]] with an [[IndexedGetter_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[IndexedGetter_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[Fold_]] with an [[IndexedFold_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[Fold_]] with an [[IndexedFold_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => self.foldMap(s)(a => indexed.runIndex.runForget((a, i))) })
  }
}

object Fold_ {
  /** create a polymorphic [[Fold_]] from Rank2TypeFoldLike encoding */
  private[proptics] def apply[S, T, A, B](f: Rank2TypeFoldLike[S, T, A, B]): Fold_[S, T, A, B] = new Fold_[S, T, A, B] { self =>
    override def apply[R](forget: Forget[R, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] = f(forget)
  }

  /** create a polymorphic [[Fold_]] from a getter function */
  final def apply[S, T, A, B](get: S => A): Fold_[S, T, A, B] =
    Fold_(fromGetRank2TypeFoldLike[S, T, A, B](get))

  /** create a polymorphic [[Fold_]] using a predicate to filter out elements of future optics composed with this [[Fold_]] */
  final def filter[A](predicate: A => Boolean): Fold_[A, A, A, A] =
    Fold_[A, A, A, A](new Rank2TypeFoldLike[A, A, A, A] {
      override def apply[R](forget: Forget[R, A, A])(implicit ev: Monoid[R]): Forget[R, A, A] =
        Forget { a =>
          if (predicate(a)) forget.runForget(a)
          else ev.empty
        }
    })

  /** create a polymorphic [[Fold_]] from [[cats.Foldable]] */
  final def fromFoldable[F[_], A, B](implicit ev0: Foldable[F]): Fold_[F[A], F[B], A, B] = new Fold_[F[A], F[B], A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, F[A], F[B]] =
      Forget(ev0.foldMap(_)(forget.runForget))

    override def foldMap[R](s: F[A])(f: A => R)(implicit ev: Monoid[R]): R = ev0.foldMap(s)(f)
  }

  /** fold both parts of a [[cats.Bifoldable]] with matching types */
  final def both[G[_, _]: Bifoldable, A, B]: Fold_[G[A, A], G[B, B], A, B] =
    Fold_(new Rank2TypeFoldLike[G[A, A], G[B, B], A, B] {
      override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, G[A, A], G[B, B]] = {
        val fold: (R, A) => R = (r, a) => r |+| forget.runForget(a)

        Forget(_.bifoldLeft(Monoid[R].empty)(fold, fold))
      }
    })

  /** polymorphic identity of a [[Fold_]] */
  final def id[S, T]: Fold_[S, T, S, T] = Fold_[S, T, S, T](identity[S] _)

  private[proptics] def fromGetRank2TypeFoldLike[S, T, A, B](get: S => A): Rank2TypeFoldLike[S, T, A, B] = new Rank2TypeFoldLike[S, T, A, B] {
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = liftForget[R, S, T, A, B](get)(forget)
  }

  private[proptics] def liftForget[R, S, T, A, B](f: S => A): Forget[R, A, B] => Forget[R, S, T] =
    forget => Forget(forget.runForget compose f)

  /** implicit conversion from [[Lens_]] to [[Fold_]] */
  implicit final def lensToFold[S, T, A, B](lens: Lens_[S, T, A, B]): Fold_[S, T, A, B] = lens.asFold

  /** implicit conversion from [[ALens_]] to [[Fold_]] */
  implicit final def aLensToFold[S, T, A, B](aLens: ALens_[S, T, A, B]): Fold_[S, T, A, B] = aLens.asFold

  /** implicit conversion from [[Prism_]] to [[Fold_]] */
  implicit final def prismToFold[S, T, A, B](prism: Prism_[S, T, A, B]): Fold_[S, T, A, B] = prism.asFold

  /** implicit conversion from [[APrism_]] to [[Fold_]] */
  implicit final def aPrismToFold[S, T, A, B](aPrism: APrism_[S, T, A, B]): Fold_[S, T, A, B] = aPrism.asFold

  /** implicit conversion from [[AffineTraversal_]] to [[Fold_]] */
  implicit final def affineTraversalToFold[S, T, A, B](affineTraversal: AffineTraversal_[S, T, A, B]): Fold_[S, T, A, B] = affineTraversal.asFold

  /** implicit conversion from [[AnAffineTraversal_]] to [[Fold_]] */
  implicit final def anAffineTraversalToFold[S, T, A, B](anAffineTraversal: AnAffineTraversal_[S, T, A, B]): Fold_[S, T, A, B] = anAffineTraversal.asFold

  /** implicit conversion from [[Traversal_]] to [[Fold_]] */
  implicit final def traversalToFold[S, T, A, B](traversal: Traversal_[S, T, A, B]): Fold_[S, T, A, B] = traversal.asFold

  /** implicit conversion from [[ATraversal_]] to [[Fold_]] */
  implicit final def aTraversalToFold[S, T, A, B](aTraversal: ATraversal_[S, T, A, B]): Fold_[S, T, A, B] = aTraversal.asFold

  /** implicit conversion from [[Getter_]] to [[Fold_]] */
  implicit final def getterToFold[S, T, A, B](getter: Getter_[S, T, A, B]): Fold_[S, T, A, B] = getter.asFold
}

object Fold {
  /** create a monomorphic [[Fold]] from a getter function */
  final def apply[S, A](f: S => A): Fold[S, A] = Fold_(f)

  /** create a monomorphic [[Fold]] using a predicate to filter out elements of future optics composed with this [[Fold_]] */
  final def filter[A](predicate: A => Boolean): Fold[A, A] = Fold_.filter(predicate)

  /** create a monomorphic [[Fold]] using a [[Fold]] to filter out elements of future optics composed with this [[Fold_]] */
  final def filter[A, B](fold: Fold[A, B]): Fold[A, A] =
    Fold_[A, A, A, A](new Rank2TypeFoldLike[A, A, A, A] {
      override def apply[R: Monoid](forget: Forget[R, A, A]): Forget[R, A, A] =
        Forget(a => fold.preview(a).fold(Monoid[R].empty)(const(forget.runForget(a))))
    })

  /** create a monomorphic [[Fold]] from [[cats.Foldable]] */
  final def fromFoldable[F[_]: Foldable, A]: Fold[F[A], A] = Fold_.fromFoldable

  /** fold both parts of a [[cats.Bifoldable]] with matching types */
  final def both[G[_, _]: Bifoldable, A]: Fold[G[A, A], A] = Fold_.both[G, A, A]

  /** monomorphic identity of a [[Fold]] */
  final def id[S]: Fold[S, S] = Fold_.id[S, S]

  /** create a monomorphic [[Fold]] that narrows the focus to a single element */
  final def single[F[_]: Foldable, A](i: Int): Fold[F[A], A] = Fold.fromFoldable[F, A].single(i)

  /** create a monomorphic [[Fold]] that selects the first n elements of a Foldable */
  final def take[G[_]: Foldable, A](i: Int): Fold[G[A], A] = Fold.fromFoldable[G, A].take(i)

  /** create a monomorphic [[Fold]] that selects all elements of a Foldable except the first n ones */
  final def drop[G[_]: Foldable, A](i: Int): Fold[G[A], A] = Fold.fromFoldable[G, A].drop(i)

  /** create a monomorphic [[Fold]] that takes the longest prefix of elements of a Foldable that satisfy a predicate */
  final def takeWhile[G[_]: Foldable, A](predicate: A => Boolean): Fold[G[A], A] =
    Fold.fromFoldable[G, A].takeWhile(predicate)

  /** create a monomorphic [[Fold]] that drop longest prefix of elements of a Foldable that satisfy a predicate */
  final def dropWhile[G[_]: Foldable, A](predicate: A => Boolean): Fold[G[A], A] =
    Fold.fromFoldable[G, A].dropWhile(predicate)

  /** check to see if a [[Fold]] matches one or more entries */
  final def has[S, A](fold: Fold[S, A]): S => Boolean = fold.nonEmpty
}
