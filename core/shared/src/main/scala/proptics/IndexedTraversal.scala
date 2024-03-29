package proptics

import cats.{Applicative, Monoid, Traverse}

import proptics.IndexedLens_.liftIndexedOptic
import proptics.IndexedTraversal_.wander
import proptics.indices.TraverseWithIndex
import proptics.internal._
import proptics.profunctor.Wander._
import proptics.profunctor.{Star, Traversing, Wander}
import proptics.rank2types.{LensLikeWithIndex, Rank2TypeIndexedTraversalLike, Rank2TypeTraversalLike}
import proptics.syntax.indexedTraversal._
import proptics.syntax.star._
import proptics.syntax.tuple._

/** An [[IndexedTraversal_]] is an indexed optic that focuses on zero or more values, and their indices.
  *
  * @tparam I
  *   the index of an [[IndexedTraversal_]]
  * @tparam S
  *   the source of an [[IndexedTraversal_]]
  * @tparam T
  *   the modified source of an [[IndexedTraversal_]]
  * @tparam A
  *   the foci of an [[IndexedTraversal_]]
  * @tparam B
  *   the modified foci of an [[IndexedTraversal_]]
  */
abstract class IndexedTraversal_[I, S, T, A, B] extends IndexedTraversal1[I, S, T, A, B] { self =>
  private[proptics] def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T]

  /** modify the foci type of an [[IndexedTraversal_]] using a function, resulting in a change of type to the full structure */
  final def over(f: ((A, I)) => B): S => T = self(Indexed(f))

  /** modify each focus of an [[IndexedTraversal_]] using a Functor, resulting in a change of type to the full structure */
  final def traverse[F[_]: Applicative](s: S)(f: ((A, I)) => F[B]): F[T] =
    self[Star[F, *, *]](Indexed(Star[F, (A, I), B](f))).runStar(s)

  /** synonym for [[asTraversal]] */
  final def unIndex: Traversal_[S, T, A, B] = asTraversal

  /** remap the index, resulting in a change of type to the full structure */
  final def reindex[J](f: I => J): IndexedTraversal_[J, S, T, A, B] = new IndexedTraversal_[J, S, T, A, B] {
    override private[proptics] def apply[P[_, _]](indexed: Indexed[P, J, A, B])(implicit ev: Wander[P]): P[S, T] =
      self(indexed.reindex[I](f)(ev))
  }

  /** transform an [[IndexedTraversal_]] to a [[Traversal_]] */
  final def asTraversal: Traversal_[S, T, A, B] = Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] =
      self(Indexed(ev.dimap[A, B, (A, I), B](pab)(_._1)(identity)))
  })

  /** transform an [[IndexedLens_]] to an [[IndexedFold_]] */
  final def asIndexedFold: IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget))
  }

  /** compose this [[IndexedTraversal_]] with an [[Iso_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with an [[Iso_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with an [[AnIso_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with an [[AnIso_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with a [[Lens_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with a [[Lens_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with an [[ALens_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with an [[ALens_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with a [[Prism_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with a [[Prism_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with an [[APrism_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with an [[APrism_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with an [[AffineTraversal_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with an [[AffineTraversal_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with an [[AnAffineTraversal_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with an [[AnAffineTraversal_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with a [[Traversal_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with a [[Traversal_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with an [[ATraversal_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.traverse(a)(c => f((c, i))) }
    })

  /** compose this [[IndexedTraversal_]] with an [[ATraversal_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[IndexedTraversal_]] with a [[Setter_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B] { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) })
  }

  /** compose this [[IndexedTraversal_]] with a [[Setter_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: Setter_[C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over(s => self.over { case (a, i) => indexed.runIndex((a, i)) }(s))
  }

  /** compose this [[IndexedTraversal_]] with a [[Getter_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedTraversal_]] with a [[Getter_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedTraversal_]] with a [[Fold_]], having this [[IndexedTraversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedTraversal_]] with a [[Fold_]], having this [[IndexedTraversal_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) }))
  }

  /** compose this [[IndexedTraversal_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = new IndexedTraversal_[J, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, J, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, J), D] = new Traversing[S, T, (C, J), D] {
        override def apply[F[_]](f: ((C, J)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, _) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose this [[IndexedTraversal_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[IndexedLens_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, I), D] = new Traversing[S, T, (C, I), D] {
        override def apply[F[_]](f: ((C, I)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, i) => other.overF { case (otherA, _) => f((otherA, i)) }(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose this [[IndexedTraversal_]] with an [[IndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = andThenWithRightIndex(other.asIndexedLens)

  /** compose this [[IndexedTraversal_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = andThenWithLeftIndex(other.asIndexedLens)

  /** compose this [[IndexedTraversal_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = new IndexedTraversal_[J, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, J, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, J), D] = new Traversing[S, T, (C, J), D] {
        override def apply[F[_]](f: ((C, J)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, _) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose this [[IndexedTraversal_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def andThenWithLeftIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, I), D] = new Traversing[S, T, (C, I), D] {
        override def apply[F[_]](f: ((C, I)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, i) => other.overF { case (otherA, _) => f((otherA, i)) }(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose this [[IndexedTraversal_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def <<*[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T =
      self(Indexed[* => *, I, A, B](other(indexed) compose Tuple2._1))
  }

  /** compose this [[IndexedTraversal_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[IndexedSetter_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B] { case (a, i) =>
        other.over { case (c, _) => indexed.runIndex((c, i)) }(a)
      })
  }

  /** compose this [[IndexedTraversal_]] with an [[IndexedSetter_]], while preserving self indices */
  final def <<*[C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] =
    andThenWithRightIndex(other.asIndexedFold)

  /** compose this [[IndexedTraversal_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose this [[IndexedTraversal_]] with an [[IndexedGetter_]], while preserving self indices */
  final def andThenWithLeftIndex[J, C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] =
    andThenWithLeftIndex(other.asIndexedFold)

  /** compose this [[IndexedTraversal_]] with an [[IndexedGetter_]], while preserving self indices */
  final def <<*[C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] = {
      val runForget = other(indexed).runForget

      Forget(self.foldMap(_)(runForget compose Tuple2._1))
    }
  }

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) =>
        other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) }
      })
  }

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]], while preserving self indices */
  final def <<*[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)
}

object IndexedTraversal_ {
  /** create a polymorphic [[IndexedTraversal_]] from Rank2TypeIndexedTraversalLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedTraversalLike[I, S, T, A, B]): IndexedTraversal_[I, S, T, A, B] = new IndexedTraversal_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = f(indexed)
  }

  /** create a polymorphic [[IndexedTraversal_]] from a getter/setter pair */
  final def apply[I, S, T, A, B](get: S => (A, I))(_set: S => B => T): IndexedTraversal_[I, S, T, A, B] = new IndexedTraversal_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (A, I), B] = new Traversing[S, T, (A, I), B] {
        override def apply[F[_]](f: ((A, I)) => F[B])(s: S)(implicit ev: Applicative[F]): F[T] =
          ev.map(f(get(s)))(_set(s))
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** create a polymorphic [[IndexedTraversal_]] from a combined getter/setter */
  final def traversal[I, S, T, A, B](to: S => ((A, I), B => T)): IndexedTraversal_[I, S, T, A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(indexed.runIndex)
    })

  /** create a polymorphic [[IndexedTraversal_]] from a [[proptics.indices.TraverseWithIndex]] */
  final def fromTraverseWithIndex[G[_], I, A, B](implicit ev0: TraverseWithIndex[G, I]): IndexedTraversal_[I, G[A], G[B], A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, G[A], G[B], A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev2: Wander[P]): P[G[A], G[B]] = {
        val traversing = new Traversing[G[A], G[B], (A, I), B] {
          override def apply[F[_]](f: ((A, I)) => F[B])(s: G[A])(implicit ev3: Applicative[F]): F[G[B]] =
            ev0.traverseWithIndex[F, A, B]((a, i) => f((a, i)))(s)
        }

        ev2.wander(traversing)(indexed.runIndex)
      }
    })

  /** create a polymorphic [[IndexedTraversal_]] from a Traverse that has an index ot type Int */
  final def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): IndexedTraversal_[Int, G[A], G[B], A, B] =
    Traversal_.fromTraverse[G, A, B].zipWithIndex

  /** create a polymorphic [[IndexedTraversal_]] from a rank 2 type traversal function */
  final def wander[I, S, T, A, B](lensLikeWithIndex: LensLikeWithIndex[I, S, T, A, B]): IndexedTraversal_[I, S, T, A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev0: Wander[P]): P[S, T] = {
        def traversing: Traversing[S, T, (A, I), B] = new Traversing[S, T, (A, I), B] {
          override def apply[F[_]](f: ((A, I)) => F[B])(s: S)(implicit ev1: Applicative[F]): F[T] = lensLikeWithIndex[F](f)(ev1)(s)
        }

        ev0.wander(traversing)(indexed.runIndex)
      }
    })
}

object IndexedTraversal {
  /** create a monomorphic [[IndexedTraversal]] from a getter/setter pair */
  final def apply[I, S, A](get: S => (A, I))(set: S => A => S): IndexedTraversal[I, S, A] = IndexedTraversal_(get)(set)

  /** create a monomorphic [[IndexedTraversal]] from a combined getter/setter. synonym for apply */
  final def traversal[I, S, A](to: S => ((A, I), A => S)): IndexedTraversal[I, S, A] = IndexedTraversal_.traversal(to)

  /** create a monomorphic [[IndexedTraversal_]] from a [[cats.Traverse]] */
  final def fromTraverseWithIndex[F[_], I, A](implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal_.fromTraverseWithIndex[F, I, A, A]

  /** create a monomorphic [[IndexedTraversal_]] from a Traverse that has an index ot type Int */
  final def fromTraverse[F[_], A](implicit ev0: Traverse[F]): IndexedTraversal[Int, F[A], A] =
    IndexedTraversal_.fromTraverse[F, A, A]

  /** create a monomorphic [[IndexedTraversal]] from a rank 2 type traversal function */
  final def wander[I, S, A](lensLikeWithIndex: LensLikeWithIndex[I, S, S, A, A]): IndexedTraversal[I, S, A] =
    IndexedTraversal_.wander[I, S, S, A, A](lensLikeWithIndex)

  /** create a monomorphic [[IndexedTraversal_]] that narrows the focus to a single element */
  final def single[F[_], A](i: Int)(implicit ev0: TraverseWithIndex[F, Int]): IndexedTraversal[Int, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, Int, A].single(i)

  /** create a monomorphic [[IndexedTraversal_]] that takes the longest prefix of elements of a Traverse that satisfy a predicate */
  final def takeWhile[F[_], I, A](predicate: A => Boolean)(implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, I, A].takeWhile(predicate)

  /** create a monomorphic [[IndexedTraversal_]] that takes the longest prefix of elements of a Traverse that satisfy a predicate */
  final def takeWhileWithIndex[F[_], I, A](predicate: ((A, I)) => Boolean)(implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, I, A].takeWhileWithIndex(predicate)

  /** create a monomorphic [[IndexedTraversal_]] that drop longest prefix of elements of a Traverse that satisfy a predicate */
  final def dropWhile[F[_], I, A](predicate: A => Boolean)(implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, I, A].dropWhile(predicate)

  /** create a monomorphic [[IndexedTraversal_]] that drop longest prefix of elements of a Traverse that satisfy a predicate */
  final def dropWhileWithIndex[F[_], I, A](predicate: ((A, I)) => Boolean)(implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, I, A].dropWhileWithIndex(predicate)
}
