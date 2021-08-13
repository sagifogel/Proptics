package proptics

import scala.Function.const

import cats.syntax.bifunctor._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Eq, Monoid}

import proptics.internal.Forget._
import proptics.internal.{Forget, Indexed}
import proptics.syntax.tuple._

/** An [[IndexedGetter_]] is an [[IndexedFold_]] without a [[cats.Monoid]].
  *
  * An [[IndexedGetter_]] is just any get function (S -> (A, I))
  *
  * @tparam I the index of an [[IndexedGetter_]]
  * @tparam S the source of an [[IndexedGetter_]]
  * @tparam T the modified source of an [[IndexedGetter_]]
  * @tparam A the focus of an [[IndexedGetter_]]
  * @tparam B the modified focus of an [[IndexedGetter_]]
  */
abstract class IndexedGetter_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(indexed: Indexed[Forget[(A, I), *, *], I, A, B]): Forget[(A, I), S, T]

  /** view the focus and the index of an [[IndexedGetter_]] */
  final def view(s: S): (A, I) = toForget.runForget(s)

  /** test whether a predicate holds for the focus of an [[IndexedGetter_]] */
  final def exists(f: ((A, I)) => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of an [[IndexedGetter_]] */
  final def notExists(f: ((A, I)) => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether a focus at specific index of an [[IndexedGetter_]] contains a given value */
  final def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedGetter_]] does not contain a given value */
  final def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)

  /** find if a focus of an [[IndexedGetter_]] that satisfies a predicate */
  final def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = s => view(s).some.find(f)

  /** synonym to [[asGetter]] */
  final def unIndex: Getter_[S, T, A, B] = asGetter

  /** remap the index, resulting in a change of type to the full structure */
  final def reindex[J](f: I => J): IndexedGetter_[J, S, T, A, B] = new IndexedGetter_[J, S, T, A, B] {
    override private[proptics] def apply(indexed: Indexed[Forget[(A, J), *, *], J, A, B]): Forget[(A, J), S, T] = {
      val forget: Forget[(A, J), (A, I), B] = indexed.reindex[I](f)(profunctorForget[(A, J)]).runIndex

      Forget(forget.runForget compose self.toForget.runForget)
    }
  }

  /** transform an [[IndexedGetter_]] to a [[Getter_]] */
  final def asGetter: Getter_[S, T, A, B] = new Getter_[S, T, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T] =
      Forget(forget.runForget compose Tuple2._1[A, I] compose self.view)
  }

  /** transform an [[IndexedGetter_]] to an [[IndexedFold_]] */
  final def asIndexedFold: IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose self.view)
  }

  /** compose this [[IndexedGetter_]] with an [[Iso_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        indexed.runIndex.runForget((other.view(a), i))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[Iso_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(indexed.runIndex.runForget compose self.view compose other.view)
  }

  /** compose this [[IndexedGetter_]] with an [[AnIso_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        indexed.runIndex.runForget((other.view(a), i))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[AnIso_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(indexed.runIndex.runForget compose self.view compose other.view)
  }

  /** compose this [[IndexedGetter_]] with a [[Lens_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        indexed.runIndex.runForget((other.view(a), i))
      }
  }

  /** compose this [[IndexedGetter_]] with a [[Lens_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(indexed.runIndex.runForget compose self.view compose other.view)
  }

  /** compose this [[IndexedGetter_]] with an [[ALens_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        indexed.runIndex.runForget((other.view(a), i))
      }
  }

  /** compose this [[IndexedGetter_]] with a [[ALens_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(indexed.runIndex.runForget compose self.view compose other.view)
  }

  /** compose this [[IndexedGetter_]] with a [[Prism_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.viewOrModify(a).fold(const(Monoid[R].empty), c => indexed.runIndex.runForget((c, i)))
      }
  }

  /** compose this [[IndexedGetter_]] with a [[Prism_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .fold(Monoid[R].empty)(s => indexed.runIndex.runForget(self.view(s)))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[APrism_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.viewOrModify(a).fold(const(Monoid[R].empty), c => indexed.runIndex.runForget((c, i)))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[APrism_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .fold(Monoid[R].empty)(s => indexed.runIndex.runForget(self.view(s)))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[AffineTraversal_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.viewOrModify(a).fold(const(Monoid[R].empty), c => indexed.runIndex.runForget((c, i)))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[AffineTraversal_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .fold(Monoid[R].empty)(s => indexed.runIndex.runForget(self.view(s)))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[AnAffineTraversal_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.viewOrModify(a).fold(const(Monoid[R].empty), c => indexed.runIndex.runForget((c, i)))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[AnAffineTraversal_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .fold(Monoid[R].empty)(s => indexed.runIndex.runForget(self.view(s)))
      }
  }

  /** compose this [[IndexedGetter_]] with a [[Traversal_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.foldMap(a)(s => indexed.runIndex.runForget((s, i)))
      }
  }

  /** compose this [[IndexedGetter_]] with a [[Traversal_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(indexed.runIndex.runForget compose self.view))
  }

  /** compose this [[IndexedGetter_]] with an [[ATraversal_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.foldMap(a)(s => indexed.runIndex.runForget((s, i)))
      }
  }

  /** compose this [[IndexedGetter_]] with an [[ATraversal_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(indexed.runIndex.runForget compose self.view))
  }

  /** compose this [[IndexedGetter_]] with a [[Getter_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        indexed.runIndex.runForget((other.view(a), i))
      }
  }

  /** compose this [[IndexedGetter_]] with a [[Getter_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (a, i) = self.view(other.view(c))
        indexed.runIndex.runForget((a, i))
      }
  }

  /** compose this [[IndexedGetter_]] with a [[Fold_]], having this [[IndexedGetter_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i)))
      }
  }

  /** compose this [[IndexedGetter_]] with a [[Fold_]], having this [[IndexedGetter_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget {
        other.foldMap(_) { s =>
          val (a, i) = self.view(s)
          indexed.runIndex.runForget((a, i))
        }
      }
  }

  /** compose an [[IndexedGetter_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = new IndexedGetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, J), *, *], J, C, D]): Forget[(C, J), S, T] =
      Forget(other.view _ compose Tuple2._1[A, I] compose self.view)
  }

  /** compose [[IndexedGetter_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[IndexedGetter_]] with an [[IndexedLens_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(self.view(_).leftMap(other.view(_)._1))
  }

  /** compose [[IndexedGetter_]] with an [[IndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[IndexedGetter_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = new IndexedGetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, J), *, *], J, C, D]): Forget[(C, J), S, T] =
      Forget(other.view _ compose Tuple2._1[A, I] compose self.view)
  }

  /** compose [[IndexedGetter_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[IndexedGetter_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(self.view(_).leftMap(other.view(_)._1))
  }

  /** compose [[IndexedGetter_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[IndexedGetter_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1)(indexed.runIndex.runForget))
  }

  /** compose [[IndexedGetter_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[IndexedGetter_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) }
      }
  }

  /** compose [[IndexedGetter_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def <<*[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose [[IndexedGetter_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = new IndexedGetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, J), *, *], J, C, D]): Forget[(C, J), S, T] =
      Forget(other.view _ compose Tuple2._1[A, I] compose self.view)
  }

  /** compose [[IndexedGetter_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose [[IndexedGetter_]] with an [[IndexedGetter_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        val (c, _) = other.view(a)

        indexed.runIndex.runForget((c, i))
      }
  }

  /** compose [[IndexedGetter_]] with an [[IndexedGetter_]], while preserving self indices */
  final def <<*[C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose [[IndexedGetter_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1)(indexed.runIndex.runForget))
  }

  /** compose [[IndexedGetter_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose [[IndexedGetter_]] with an [[IndexedFold_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) }
      }
  }

  /** compose [[IndexedGetter_]] with an [[IndexedFold_]], while preserving self indices */
  final def <<*[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)

  private def toForget: Forget[(A, I), S, T] = self(Indexed(Forget(identity)))
}

object IndexedGetter_ {
  /** create a polymorphic [[IndexedGetter_]] from a indexed [[Forget]] function */
  private[proptics] def apply[I, S, T, A, B](f: Indexed[Forget[(A, I), *, *], I, A, B] => Forget[(A, I), S, T])(implicit ev: DummyImplicit): IndexedGetter_[I, S, T, A, B] =
    new IndexedGetter_[I, S, T, A, B] {
      override def apply(indexed: Indexed[Forget[(A, I), *, *], I, A, B]): Forget[(A, I), S, T] = f(indexed)
    }

  /** create a polymorphic [[IndexedGetter_]] from a getter function */
  final def apply[I, S, T, A, B](get: S => (A, I)): IndexedGetter_[I, S, T, A, B] =
    IndexedGetter_ { indexed: Indexed[Forget[(A, I), *, *], I, A, B] => Forget[(A, I), S, T](indexed.runIndex.runForget compose get) }
}

object IndexedGetter {
  /** create a monomorphic [[IndexedGetter]] from a getter function */
  final def apply[I, S, A](get: S => (A, I)): IndexedGetter[I, S, A] = IndexedGetter_(get)
}
