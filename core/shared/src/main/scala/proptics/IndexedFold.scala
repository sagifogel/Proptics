package proptics

import cats.{Foldable, Monoid}

import proptics.indices.FoldableWithIndex
import proptics.internal.{Forget, Indexed}
import proptics.rank2types.Rank2TypeIndexedFoldLike
import proptics.syntax.tuple._

/** A [[IndexedFold_]] is a generalization of something Foldable. It describes how to retrieve multiple values and thier indices.
  *
  * A [[IndexedFold_]] is similar to a [[IndexedTraversal_]], but it cannot modify its foci.
  *
  * An [[IndexedFold_]] is an indexed optic with fixed type [[proptics.internal.Forget]] [[cats.arrow.Profunctor]]
  *
  * @tparam I
  *   the index of an [[IndexedFold_]]
  * @tparam S
  *   the source of an [[IndexedFold_]]
  * @tparam T
  *   the modified source of an [[IndexedFold_]]
  * @tparam A
  *   the foci of an [[IndexedFold_]]
  * @tparam B
  *   the modified foci of an [[IndexedFold_]]
  */
abstract class IndexedFold_[I, S, T, A, B] extends IndexedFoldCompat[I, S, A] { self =>
  private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]

  /** synonym to [[fold]] */
  final def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  /** map each focus of an [[IndexedFold_]] to a [[cats.Monoid]], and combine the results */
  final def foldMap[R: Monoid](s: S)(f: ((A, I)) => R): R = self[R](Indexed(Forget(f))).runForget(s)

  /** synonym to [[asFold]] */
  final def unIndex: Fold_[S, T, A, B] = asFold

  /** remap the index, resulting in a change of type to the full structure */
  final def reindex[J](f: I => J): IndexedFold_[J, S, T, A, B] = new IndexedFold_[J, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, A, B]): Forget[R, S, T] =
      self(indexed.reindex[I](f)(Forget.profunctorForget[R]))
  }

  /** transform an [[IndexedFold_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose Tuple2._1))
  }

  /** compose this [[IndexedFold_]] with an [[Iso_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with an [[Iso_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[AnIso_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with an [[AnIso_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[Lens_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with an [[Lens_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[ALens_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with an [[ALens_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[Prism_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[Prism_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .fold(Monoid[R].empty)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) })
      }
  }

  /** compose this [[IndexedFold_]] with an [[APrism_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[APrism_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .fold(Monoid[R].empty)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) })
      }
  }

  /** compose this [[IndexedFold_]] with an [[AffineTraversal_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[AffineTraversal_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .fold(Monoid[R].empty)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) })
      }
  }

  /** compose this [[IndexedFold_]] with an [[AffineTraversal_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[AffineTraversal_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .fold(Monoid[R].empty)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) })
      }
  }

  /** compose this [[IndexedFold_]] with an [[Traversal_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[Traversal_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) }))
  }

  /** compose this [[IndexedFold_]] with an [[ATraversal_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[ATraversal_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) }))
  }

  /** compose this [[IndexedFold_]] with a [[Getter_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with a [[Getter_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with a function lifted to a [[Getter_]], having this [[IndexedFold_]] applied first */
  final def focus[C, D](f: A => C): IndexedFold_[I, S, T, C, D] = andThen(Getter_[A, B, C, D](f))

  /** compose this [[IndexedFold_]] with a [[Fold_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with a [[Fold_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) }))
  }

  /** compose this [[IndexedFold_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._1))
  }

  /** compose [[IndexedFold_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedLens_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[IndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._1))
  }

  /** compose [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) } })
  }

  /** compose this [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def <<*[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => indexed.runIndex.runForget(other.view(a)) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedGetter_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[IndexedGetter_]], while preserving self indices */
  final def <<*[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedFold_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) } })
  }

  /** compose this [[IndexedFold_]] with an [[IndexedFold_]], while preserving self indices */
  final def <<*[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with a function lifted to an [[IndexedGetter_]] */
  final def toWithIndex[C, D](f: A => (C, I)): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(IndexedGetter_[I, A, B, C, D](f))
}

object IndexedFold_ {
  /** create a polymorphic [[IndexedFold_]] from Rank2TypeIndexedFoldLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedFoldLike[I, S, T, A, B])(implicit ev: DummyImplicit): IndexedFold_[I, S, T, A, B] =
    new IndexedFold_[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] = f(indexed)
    }

  /** create a polymorphic [[IndexedFold_]] from a getter function */
  final def apply[I, S, T, A, B](get: S => (A, I)): IndexedFold_[I, S, T, A, B] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] =
        Forget(indexed.runIndex.runForget compose get)
    })

  /** create a polymorphic [[IndexedFold_]] from Foldable that has an index ot type Int */
  final def fromFoldable[F[_], A, B](implicit ev0: Foldable[F]): IndexedFold_[Int, F[A], F[B], A, B] =
    Fold_.fromFoldable[F, A, F[B], B].asIndexableFold

  /** create a polymorphic [[IndexedFold_]] from [[proptics.indices.FoldableWithIndex]] */
  final def fromFoldableWithIndex[F[_], I, A, B](implicit ev: FoldableWithIndex[F, I]): IndexedFold_[I, F[A], F[B], A, B] = new IndexedFold_[I, F[A], F[B], A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, F[A], F[B]] =
      Forget(fa => ev.foldMapWithIndex[A, R]((a, i) => indexed.runIndex.runForget((a, i)))(fa))
  }

  /** implicit conversion from [[IndexedLens_]] to [[IndexedFold_]] */
  implicit def indexedLensToIndexedFold[I, S, T, A, B](indexedLens: IndexedLens_[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = indexedLens.asIndexedFold

  /** implicit conversion from [[AnIndexedLens_]] to [[IndexedFold_]] */
  implicit def anIndexedLensToIndexedFold[I, S, T, A, B](anIndexedLens: AnIndexedLens_[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = anIndexedLens.asIndexedFold

  /** implicit conversion from [[Traversal_]] to [[IndexedFold_]] */
  implicit def indexedTraversalToIndexedFold[I, S, T, A, B](indexedTraversal: IndexedTraversal_[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = indexedTraversal.asIndexedFold

  /** implicit conversion from [[IndexedGetter_]] to [[IndexedFold_]] */
  implicit def indexedGetterToIndexedFold[I, S, T, A, B](indexedGetter: IndexedGetter_[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = indexedGetter.asIndexedFold
}

object IndexedFold {
  /** create a monomorphic [[IndexedFold]] from a getter function */
  final def apply[I, S, A](get: S => (A, I)): IndexedFold[I, S, A] = IndexedFold_(get)

  /** create a monomorphic [[IndexedFold]] using a predicate to filter out elements of future optics composed with this [[IndexedFold_]] */
  final def filtered[I, A](predicate: ((A, I)) => Boolean): IndexedFold_[I, (A, I), (A, I), A, A] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, (A, I), (A, I), A, A] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, A])(implicit ev: Monoid[R]): Forget[R, (A, I), (A, I)] =
        Forget { p =>
          if (predicate(p)) indexed.runIndex.runForget((p._1, p._2))
          else ev.empty
        }
    })

  /** create a monomorphic [[IndexedFold]] from [[proptics.indices.FoldableWithIndex]] */
  final def fromFoldableWithIndex[F[_], I, A](implicit ev: FoldableWithIndex[F, I]): IndexedFold[I, F[A], A] =
    IndexedFold_.fromFoldableWithIndex[F, I, A, A]

  /** create a monomorphic [[IndexedFold]] from Foldable */
  final def fromFoldable[F[_], A](implicit ev: Foldable[F]): IndexedFold[Int, F[A], A] = IndexedFold_.fromFoldable[F, A, A]

  /** check to see if an [[IndexedFold]] matches one or more entries */
  final def has[I, S, A](indexedFold: IndexedFold[I, S, A]): S => Boolean = indexedFold.nonEmpty
}
