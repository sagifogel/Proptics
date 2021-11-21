package proptics

import scala.Function.const

import cats.Monoid
import cats.syntax.option._

import proptics.internal.{Forget, Getter1, Indexed}

/** A [[Getter_]] is a [[Fold_]] without a [[cats.Monoid]].
  *
  * A [[Getter_]] is just any get function (S -> A)
  *
  * @tparam S
  *   the source of a [[Getter_]]
  * @tparam T
  *   the modified source of a [[Getter_]]
  * @tparam A
  *   the focus of a [[Getter_]]
  * @tparam B
  *   the modified focus of a [[Getter_]]
  */
abstract class Getter_[S, T, A, B] extends Getter1[S, A] { self =>
  private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T]

  /** view the focus of a [[Getter_]] */
  final def view(s: S): A = self(Forget(identity)).runForget(s)

  /** compose this [[Getter_]] with a function lifted to a [[Getter_]] */
  final def focus[C, D](f: A => C): Getter_[S, T, C, D] = andThen(Getter_[A, B, C, D](f))

  /** transform a [[Getter_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(forget.runForget compose self.view)
  }

  /** compose this [[Getter_]] with an [[Iso_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  /** compose this [[Getter_]] with an [[Iso_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): Getter_[C, D, A, B] = new Getter_[C, D, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, C, D] =
      Forget(forget.runForget compose self.view compose other.view)
  }

  /** compose this [[Getter_]] with an [[AnIso_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  /** compose this [[Getter_]] with an [[AnIso_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): Getter_[C, D, A, B] = new Getter_[C, D, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, C, D] =
      Forget(forget.runForget compose self.view compose other.view)
  }

  /** compose this [[Getter_]] with a [[Lens_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  /** compose this [[Getter_]] with a [[Lens_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): Getter_[C, D, A, B] = new Getter_[C, D, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, C, D] =
      Forget(forget.runForget compose self.view compose other.view)
  }

  /** compose this [[Getter_]] with an [[ALens_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  /** compose this [[Getter_]] with an [[ALens_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): Getter_[C, D, A, B] = new Getter_[C, D, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, C, D] =
      Forget(forget.runForget compose self.view compose other.view)
  }

  /** compose this [[Getter_]] with a [[Prism_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.preview(self.view(s)).fold(Monoid[R].empty)(forget.runForget))
  }

  /** compose this [[Getter_]] with a [[Prism_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.viewOrModify(_).fold(const(Monoid[R].empty), forget.runForget compose self.view))
  }

  /** compose this [[Getter_]] with an [[APrism_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.preview(self.view(s)).fold(Monoid[R].empty)(forget.runForget))
  }

  /** compose this [[Getter_]] with an [[APrism_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.viewOrModify(_).fold(const(Monoid[R].empty), forget.runForget compose self.view))
  }

  /** compose this [[Getter_]] with an [[AffineTraversal_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.preview(self.view(s)).fold(Monoid[R].empty)(forget.runForget))
  }

  /** compose this [[Getter_]] with an [[AffineTraversal_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.viewOrModify(_).fold(const(Monoid[R].empty), forget.runForget compose self.view))
  }

  /** compose this [[Getter_]] with an [[AnAffineTraversal_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.preview(self.view(s)).fold(Monoid[R].empty)(forget.runForget))
  }

  /** compose this [[Getter_]] with an [[AnAffineTraversal_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.viewOrModify(_).fold(const(Monoid[R].empty), forget.runForget compose self.view))
  }

  /** compose this [[Getter_]] with a [[Traversal_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(forget.runForget))
  }

  /** compose this [[Getter_]] with a [[Traversal_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(forget.runForget compose self.view))
  }

  /** compose this [[Getter_]] with an [[ATraversal_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(forget.runForget))
  }

  /** compose this [[Getter_]] with an [[ATraversal_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(forget.runForget compose self.view))
  }

  /** compose this [[Getter_]] with a [[Getter_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  /** compose this [[Getter_]] with a [[Getter_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): Getter_[C, D, A, B] = new Getter_[C, D, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, C, D] =
      Forget(forget.runForget compose self.view compose other.view)
  }

  /** compose this [[Getter_]] with a [[Fold_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(forget.runForget))
  }

  /** compose this [[Getter_]] with a [[Fold_]], having this [[Getter_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(forget.runForget compose self.view))
  }

  /** compose this [[Getter_]] with an [[IndexedLens_]], having this [[Getter_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(indexed.runIndex.runForget compose other.view compose self.view)
  }

  /** compose this [[Getter_]] with an [[IndexedLens_]], having this [[Getter_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedGetter_[I, C, D, A, B] = new IndexedGetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[Forget[(A, I), *, *], I, A, B]): Forget[(A, I), C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        indexed.runIndex.runForget((self.view(s), i))
      }
  }

  /** compose this [[Getter_]] with an [[AnIndexedLens_]], having this [[Getter_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(indexed.runIndex.runForget compose other.view compose self.view)
  }

  /** compose this [[Getter_]] with an [[AnIndexedLens_]], having this [[Getter_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): IndexedGetter_[I, C, D, A, B] = new IndexedGetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[Forget[(A, I), *, *], I, A, B]): Forget[(A, I), C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        indexed.runIndex.runForget((self.view(s), i))
      }
  }

  /** compose this [[Getter_]] with an [[IndexedTraversal_]], having this [[Getter_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(indexed.runIndex.runForget))
  }

  /** compose this [[Getter_]] with an [[IndexedTraversal_]], having this [[Getter_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => indexed.runIndex.runForget((self.view(s), i)) })
  }

  /** compose this [[Getter_]] with an [[IndexedGetter_]], having this [[Getter_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose other.view compose self.view)
  }

  /** compose this [[Getter_]] with an [[IndexedGetter_]], having this [[Getter_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        indexed.runIndex.runForget((self.view(s), i))
      }
  }

  /** compose this [[Getter_]] with an [[IndexedFold_]], having this [[Getter_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(indexed.runIndex.runForget))
  }

  /** compose this [[Getter_]] with an [[IndexedFold_]], having this [[Getter_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => indexed.runIndex.runForget((self.view(s), i)) })
  }

  override protected def viewOption(s: S): Option[A] = view(s).some
}

object Getter_ {
  /** create a polymorphic [[Getter_]] from a [[Getter_.apply]] function */
  private[Getter_] def apply[S, T, A, B](f: Forget[A, A, B] => Forget[A, S, T])(implicit ev: DummyImplicit): Getter_[S, T, A, B] =
    new Getter_[S, T, A, B] {
      override def apply(forget: Forget[A, A, B]): Forget[A, S, T] = f(forget)
    }

  /** create a polymorphic [[Getter_]] from a getter function */
  final def apply[S, T, A, B](f: S => A): Getter_[S, T, A, B] =
    Getter_((forget: Forget[A, A, B]) => Forget[A, S, T](forget.runForget compose f))

  /** polymorphic identity of a [[Getter_]] */
  final def id[S, T]: Getter_[S, T, S, T] = Getter_[S, T, S, T](identity[S] _)
}

object Getter {
  /** create a monomorphic [[Getter]] from a getter function */
  final def apply[S, A](f: S => A): Getter[S, A] = Getter_(f)

  /** polymorphic identity of a [[Getter]] */
  final def id[S]: Getter[S, S] = Getter_.id[S, S]
}
