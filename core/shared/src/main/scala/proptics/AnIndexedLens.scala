package proptics

import scala.Function.const

import cats.arrow.Strong
import cats.data.State
import cats.syntax.apply._
import cats.syntax.bifunctor._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, catsInstancesForId}

import proptics.IndexedLens_.liftIndexedOptic
import proptics.IndexedTraversal_.wander
import proptics.internal._
import proptics.rank2types.{LensLikeWithIndex, Rank2TypeIndexedLensLike}
import proptics.syntax.tuple._

/** An [[AnIndexedLens_]] ] focuses a single piece of data and index within a larger structure.
  *
  * An [[AnIndexedLens_]] provides a convenient way to view, set, and transform that element.
  *
  * An [[AnIndexedLens_]] must never fail to get or modify that focus.
  *
  * an [[AnIndexedLens_]] is an [[IndexedLens_]] with fixed type [[proptics.internal.Shop]] [[cats.arrow.Profunctor]]
  *
  * @tparam I
  *   the index of an [[AnIndexedLens_]]
  * @tparam S
  *   the source of an [[AnIndexedLens_]]
  * @tparam T
  *   the modified source of an [[AnIndexedLens_]]
  * @tparam A
  *   the focus of an [[AnIndexedLens_]]
  * @tparam B
  *   the modified focus of an [[AnIndexedLens_]]
  */
abstract class AnIndexedLens_[I, S, T, A, B] { self =>
  private[proptics] def apply(indexed: Indexed[Shop[(A, I), B, *, *], I, A, B]): Shop[(A, I), B, S, T]

  /** view the focus and the index of an [[AnIndexedLens_]] */
  final def view(s: S): (A, I) = toShop.view(s)

  /** set the modified focus of an [[AnIndexedLens_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[AnIndexedLens_]] using a function, resulting in a change of type to the full structure */
  final def over(f: ((A, I)) => B): S => T = overF[Id](f)

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: ((A, I)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AnIndexedLens_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  final def traverse[F[_]](s: S)(f: ((A, I)) => F[B])(implicit ev: Applicative[F]): F[T] = {
    val shop = toShop
    ev.map(f(shop.view(s)))(shop.set(s)(_))
  }

  /** test whether a predicate holds for the focus of an [[AnIndexedLens_]] */
  final def exists(f: ((A, I)) => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of an [[AnIndexedLens_]] */
  final def notExists(f: ((A, I)) => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether a focus at specific index of an [[AnIndexedLens_]] contains a given value */
  final def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[AnIndexedLens_]] does not contain a given value */
  final def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)

  /** find if a focus of an [[AnIndexedLens_]] that satisfies a predicate. */
  final def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = s => view(s).some.filter(f)

  /** view the focus and the index of an [[AnIndexedLens_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, (A, I)] = ev.inspect(view)

  /** convert an [[AnIndexedLens_]] to the pair of functions that characterize it */
  final def withIndexedLens[R](f: (S => (A, I)) => (S => B => T) => R): R = {
    val shop = toShop

    f(shop.view)(shop.set)
  }

  /** convert an [[AnIndexedLens_]] to a Shop[(A, I), B, S, T] */
  final def toShop: Shop[(A, I), B, S, T] = self(Indexed(Shop(identity, const(identity))))

  /** transform an [[AnIndexedLens_]] to an [[IndexedLens_]] */
  final def asIndexedLens: IndexedLens_[I, S, T, A, B] = withIndexedLens(IndexedLens_[I, S, T, A, B])

  /** synonym to [[asLens]] */
  final def unindex: Lens_[S, T, A, B] = asLens

  /** remap the index, resulting in a change of type to the full structure */
  final def reindex[J](f: I => J): AnIndexedLens_[J, S, T, A, B] = new AnIndexedLens_[J, S, T, A, B] {
    override def apply(indexed: Indexed[Shop[(A, J), B, *, *], J, A, B]): Shop[(A, J), B, S, T] = {
      val shop: Shop[(A, J), B, (A, I), B] = indexed.reindex[I](f).runIndex

      Shop(shop.view compose self.view, s => b => self.set(shop.set(self.view(s))(b))(s))
    }
  }

  /** transform an [[AnIndexedLens_]] to an [[Lens_]] */
  final def asLens: Lens_[S, T, A, B] = withIndexedLens(sia => sbt => Lens_.lens(s => (sia(s)._1, sbt(s))))

  /** transform an [[IndexedLens_]] to an [[IndexedFold_]] */
  final def asIndexedFold: IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose self.view)
  }

  /** compose this [[AnIndexedLens_]] with an [[Iso_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_((s: S) => self.view(s).leftMap(other.view))(s => d => self.set(other.set(d)(self.view(s)._1))(s))

  /** compose this [[AnIndexedLens_]] with an [[Iso_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): AnIndexedLens_[I, C, D, A, B] =
    AnIndexedLens_((c: C) => self.view(other.view(c)))(c => b => other.review(self.set(b)(other.view(c))))

  /** compose this [[AnIndexedLens_]] with an [[AnIso_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_((s: S) => self.view(s).leftMap(other.view))(s => d => self.set(other.set(d)(self.view(s)._1))(s))

  /** compose this [[AnIndexedLens_]] with an [[AnIso_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): AnIndexedLens_[I, C, D, A, B] =
    AnIndexedLens_((c: C) => self.view(other.view(c)))(c => b => other.review(self.set(b)(other.view(c))))

  /** compose this [[AnIndexedLens_]] with a [[Lens_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_((s: S) => self.view(s).leftMap(other.view))(s => d => self.set(other.set(d)(self.view(s)._1))(s))

  /** compose this [[AnIndexedLens_]] with a [[Lens_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): AnIndexedLens_[I, C, D, A, B] =
    AnIndexedLens_((c: C) => self.view(other.view(c)))(c => b => other.set(self.set(b)(other.view(c)))(c))

  /** compose this [[AnIndexedLens_]] with an [[ALens_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_((s: S) => self.view(s).leftMap(other.view))(s => d => self.set(other.set(d)(self.view(s)._1))(s))

  /** compose this [[AnIndexedLens_]] with an [[ALens_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): AnIndexedLens_[I, C, D, A, B] =
    AnIndexedLens_((c: C) => self.view(other.view(c)))(c => b => other.set(self.set(b)(other.view(c)))(c))

  /** compose this [[AnIndexedLens_]] with a [[Prism_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF(c => f((c, i)))(a) }
    })

  /** compose this [[AnIndexedLens_]] with a [[Prism_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[AnIndexedLens_]] with an [[APrism_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF(c => f((c, i)))(a) }
    })

  /** compose this [[AnIndexedLens_]] with an [[APrism_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[AnIndexedLens_]] with an [[AffineTraversal_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF(c => f((c, i)))(a) }
    })

  /** compose this [[AnIndexedLens_]] with an [[AffineTraversal_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[AnIndexedLens_]] with an [[AnAffineTraversal_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF(c => f((c, i)))(a) }
    })

  /** compose this [[AnIndexedLens_]] with an [[AnAffineTraversal_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[AnIndexedLens_]] with a [[Traversal_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF(c => f((c, i)))(a) }
    })

  /** compose this [[AnIndexedLens_]] with a [[Traversal_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[AnIndexedLens_]] with an [[ATraversal_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF(c => f((c, i)))(a) }
    })

  /** compose this [[AnIndexedLens_]] with an [[ATraversal_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF { case (a, i) => f((a, i)) })
    })

  /** compose this [[AnIndexedLens_]] with a [[Setter_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, (C, I), D] = new Setter_[S, T, (C, I), D] {
    override private[proptics] def apply(pab: ((C, I)) => D): S => T =
      self.over { case (a, i) => other.over(c => pab((c, i)))(a) }
  }

  /** compose this [[AnIndexedLens_]] with a [[Setter_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: Setter_[C, D, S, T]): Setter_[C, D, (A, I), B] = new Setter_[C, D, (A, I), B] {
    override private[proptics] def apply(pab: ((A, I)) => B): C => D =
      other.over(self.over { case (a, i) => pab((a, i)) })
  }

  /** compose this [[AnIndexedLens_]] with a [[Getter_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, (C, I), D] = new Getter_[S, T, (C, I), D] {
    override private[proptics] def apply(forget: Forget[(C, I), (C, I), D]): Forget[(C, I), S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        forget.runForget((other.view(a), i))
      }
  }

  /** compose this [[AnIndexedLens_]] with a [[Getter_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): Getter_[C, D, (A, I), B] = new Getter_[C, D, (A, I), B] {
    override private[proptics] def apply(forget: Forget[(A, I), (A, I), B]): Forget[(A, I), C, D] =
      Forget { c =>
        val (a, i) = self.view(other.view(c))
        forget.runForget((a, i))
      }
  }

  /** compose this [[AnIndexedLens_]] with a [[Fold_]], having this [[AnIndexedLens_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, (C, I), D] = new Fold_[S, T, (C, I), D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, (C, I), D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.foldMap(a)(c => forget.runForget((c, i)))
      }
  }

  /** compose this [[AnIndexedLens_]] with a [[Fold_]], having this [[AnIndexedLens_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, (A, I), B] = new Fold_[C, D, (A, I), B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, (A, I), B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(forget.runForget compose self.view))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = new AnIndexedLens_[J, S, T, C, D] {
    final def apply(indexed: Indexed[Shop[(C, J), D, *, *], J, C, D]): Shop[(C, J), D, S, T] =
      Shop(other.view _ compose Tuple2._1[A, I] compose self.view, s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    final def apply(indexed: Indexed[Shop[(C, I), D, *, *], I, C, D]): Shop[(C, I), D, S, T] =
      Shop(self.view(_).leftMap(other.view(_)._1), s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: IndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = new AnIndexedLens_[J, S, T, C, D] {
    final def apply(indexed: Indexed[Shop[(C, J), D, *, *], J, C, D]): Shop[(C, J), D, S, T] =
      Shop(other.view _ compose Tuple2._1[A, I] compose self.view, s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def andThenWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    final def apply(indexed: Indexed[Shop[(C, I), D, *, *], I, C, D]): Shop[(C, I), D, S, T] =
      Shop(self.view(_).leftMap(other.view(_)._1), s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: AnIndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[J, S, T, C, D] {
      override def apply[F[_]](f: ((C, J)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, _) => other.overF(f)(a) }
    })

  /** compose an [[AnIndexedLens_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF { case (c, _) => f((c, i)) }(a) }
    })

  /** compose an [[AnIndexedLens_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T = s => self.set(other.over(indexed.runIndex)(self.view(s)._1))(s)
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedSetter_]], while preserving self indices */
  final def andThenWithLeftIndex[J, C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T = s => {
      val (a, i) = self.view(s)

      self.set(other.over { case (c, _) => indexed.runIndex((c, i)) }(a))(s)
    }
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedSetter_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = new IndexedGetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, J), *, *], J, C, D]): Forget[(C, J), S, T] =
      Forget(other.view _ compose Tuple2._1[A, I] compose self.view)
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedGetter_]], while preserving self indices */
  final def andThenWithLeftIndex[J, C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(s => self.view(s).leftMap(other.view(_)._1))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedGetter_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = andThenWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def andThenWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1)(indexed.runIndex.runForget))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = andThenWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedFold_]], while preserving self indices */
  final def andThenWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1) { case (c, _) => indexed.runIndex.runForget((c, self.view(s)._2)) })
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedFold_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = andThenWithLeftIndex(other)
}

object AnIndexedLens_ {
  /** create a polymorphic [[AnIndexedLens_]] from Rank2TypeIndexedLensLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): AnIndexedLens_[I, S, T, A, B] = new AnIndexedLens_[I, S, T, A, B] {
    override def apply(indexed: Indexed[Shop[(A, I), B, *, *], I, A, B]): Shop[(A, I), B, S, T] =
      f(indexed.runIndex)
  }

  /** create a polymorphic [[AnIndexedLens_]] from a getter/setter pair */
  final def apply[I, S, T, A, B](get: S => (A, I))(_set: S => B => T): AnIndexedLens_[I, S, T, A, B] =
    AnIndexedLens_.lens((get, _set).mapN(Tuple2.apply))

  /** create a polymorphic [[AnIndexedLens_]] from a combined getter/setter */
  final def lens[I, S, T, A, B](to: S => ((A, I), B => T)): AnIndexedLens_[I, S, T, A, B] =
    AnIndexedLens_(new Rank2TypeIndexedLensLike[I, S, T, A, B] {
      override def apply[P[_, _]](piab: P[(A, I), B])(implicit ev: Strong[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(piab)
    })
}

object AnIndexedLens {
  /** create a monomorphic [[AnIndexedLens]] from a getter/setter pair */
  final def apply[I, S, A](get: S => (A, I))(set: S => A => S): AnIndexedLens[I, S, A] = AnIndexedLens_(get)(set)

  /** create a monomorphic [[AnIndexedLens]] from a combined getter/setter */
  final def lens[I, S, A](to: S => ((A, I), A => S)): AnIndexedLens[I, S, A] = AnIndexedLens_.lens(to)
}
