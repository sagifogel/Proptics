package proptics
import scala.Function.{const, untupled}

import proptics.internal.Indexed
import proptics.syntax.tuple._

/** An [[IndexedSetter_]] is a generalization of mapWithIndex from [[proptics.indices.FunctorWithIndex]]
  *
  * @tparam I
  *   the index of an [[IndexedSetter_]]
  * @tparam S
  *   the source of an [[IndexedSetter_]]
  * @tparam T
  *   the modified source of an [[IndexedSetter_]] —
  * @tparam A
  *   the focus an [[IndexedSetter_]]
  * @tparam B
  *   the modified focus of an [[IndexedSetter_]]
  */
abstract class IndexedSetter_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): S => T

  /** set the modified focus of an [[IndexedSetter_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[IndexedSetter_]] using a function, resulting in a change of type to the full structure */
  final def over(f: ((A, I)) => B): S => T = self(Indexed(f))

  /** synonym to [[asSetter]] */
  final def unIndex: Setter_[S, T, A, B] = asSetter

  /** remap the index, resulting in a change of type to the full structure */
  final def reindex[J](f: I => J): IndexedSetter_[J, S, T, A, B] = new IndexedSetter_[J, S, T, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, A, B]): S => T = self(indexed.reindex[I](f))
  }

  /** transform an [[IndexedSetter_]] to a [[Setter_]] */
  final def asSetter: Setter_[S, T, A, B] = new Setter_[S, T, A, B] {
    override private[proptics] def apply(pab: A => B): S => T =
      self(Indexed(pab compose Tuple2._1[A, I]))
  }

  /** compose this [[IndexedSetter_]] with an [[Iso_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with an [[Iso_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with an [[AnIso_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with an [[AnIso_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with a [[Lens_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with a [[Lens_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with an [[ALens_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with an [[ALens_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with a [[Prism_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with a [[Prism_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with an [[AffineTraversal_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with an [[AffineTraversal_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with an [[AnAffineTraversal_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with an [[AnAffineTraversal_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with a [[Traversal_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with a [[Traversal_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with an [[ATraversal_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with an [[ATraversal_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with a [[Setter_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with a [[Setter_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: Setter_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose this [[IndexedSetter_]] with a [[Grate_]], having this [[IndexedSetter_]] applied first */
  final def andThen[C, D](other: Grate_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose this [[IndexedSetter_]] with a [[Grate_]], having this [[IndexedSetter_]] applied last */
  final def compose[C, D](other: Grate_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(self.over { case (a, i) => indexed.runIndex((a, i)) })
  }

  /** compose an [[IndexedSetter_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T =
      self(Indexed(other(indexed) compose Tuple2._1[A, I]))
  }

  /** compose an [[IndexedLens_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[IndexedSetter_]] with an [[IndexedLens_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over { case (c, _) => indexed.runIndex((c, i)) }(a) }
  }

  /** compose an [[IndexedLens_]] with an [[IndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[IndexedSetter_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T =
      self.over { case (a, _) => other.over { case (c, j) => indexed.runIndex((c, j)) }(a) }
  }

  /** compose an [[IndexedSetter_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def *>>[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[IndexedSetter_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over { case (c, _) => indexed.runIndex((c, i)) }(a) }
  }

  /** compose an [[IndexedLens_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def <<*[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T =
      self(Indexed(other(indexed) compose Tuple2._1[A, I]))
  }

  /** compose an [[IndexedSetter_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[IndexedSetter_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over { case (c, _) => indexed.runIndex((c, i)) }(a) }
  }

  /** compose an [[IndexedLens_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  final def andThenWithRightIndex[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T =
      self(Indexed(other(indexed) compose Tuple2._1[A, I]))
  }

  /** compose an [[IndexedSetter_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def *>>[C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[IndexedSetter_]] with an [[IndexedSetter_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over { case (c, _) => indexed.runIndex((c, i)) }(a) }
  }

  /** compose an [[IndexedLens_]] with an [[IndexedSetter_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

}

object IndexedSetter_ {
  /** create a polymorphic [[IndexedSetter_]] from an Indexed mapping function */
  private[proptics] def apply[I, S, T, A, B](mapping: Indexed[* => *, I, A, B] => S => T)(implicit ev: DummyImplicit): IndexedSetter_[I, S, T, A, B] =
    new IndexedSetter_[I, S, T, A, B] {
      override def apply(indexed: Indexed[* => *, I, A, B]): S => T = mapping(indexed)
    }

  /** create a polymorphic [[IndexedSetter_]] from an indexed mapping function */
  final def apply[I, S, T, A, B](mapping: ((A, I) => B) => S => T): IndexedSetter_[I, S, T, A, B] =
    IndexedSetter_ { indexed: Indexed[* => *, I, A, B] => mapping(untupled(indexed.runIndex)) }
}

object IndexedSetter {
  /** create a monomorphic [[IndexedSetter]] from an indexed mapping function */
  final def apply[I, S, A](mapping: ((A, I) => A) => S => S): IndexedSetter[I, S, A] = IndexedSetter_(mapping)
}
