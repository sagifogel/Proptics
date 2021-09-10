package proptics

import scala.Function.const

import cats.arrow.Strong
import cats.data.Const
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.{Applicative, Monoid}

import proptics.IndexedTraversal_.wander
import proptics.data.First
import proptics.internal.{Forget, Indexed, RunBazaar}
import proptics.profunctor.{Choice, Star, Wander}
import proptics.rank2types.{LensLikeWithIndex, Rank2TypeTraversalLike}
import proptics.syntax.star._

/** [[AffineTraversal_]] has at most one focus, but is not a [[Prism_]]
  *
  * @tparam S
  *   the source of an [[AffineTraversal_]]
  * @tparam T
  *   the modified source of an [[AffineTraversal_]]
  * @tparam A
  *   the focus of an [[AffineTraversal_]]
  * @tparam B
  *   the modified focus of an [[AffineTraversal_]]
  */
abstract class AffineTraversal_[S, T, A, B] extends FoldCompat0[S, A] { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T]

  /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
  def viewOrModify(s: S): Either[T, A]

  /** view an optional focus of an [[AffineTraversal_]] */
  final def preview(s: S): Option[A] = foldMap(s)(a => First(a.some)).runFirst

  /** set the modified focus of an [[AffineTraversal_]] */
  final def set(b: B): S => T = over(const(b))

  /** set the focus of an [[AffineTraversal_]] conditionally if it is not None */
  final def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus type of an [[AffineTraversal_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = self(f)

  /** modify the focus of an [[AffineTraversal_]] using a function conditionally if it is not None, resulting in a change of type to the full structure */
  final def overOption(f: A => B): S => Option[T] = s => preview(s).map(a => set(f(a))(s))

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AffineTraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  final def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  /** check if the [[AffineTraversal_]] does not contain a focus */
  final def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[AffineTraversal_]] contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** find if the focus of an [[AffineTraversal_]] is satisfying a predicate. */
  final def find(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  /** transform an [[AffineTraversal_]] to a [[Traversal_]] */
  final def asTraversal: Traversal_[S, T, A, B] =
    Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
      final override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = self(pab)
    })

  /** transform an [[AffineTraversal_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    final override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget))
  }

  /** compose this [[AffineTraversal_]] with a function lifted to a [[Getter_]], having this [[AffineTraversal_]] applied first */
  final def to[C, D](f: A => C): Fold_[S, T, C, D] = andThen(Getter_[A, B, C, D](f))

  /** compose this [[AffineTraversal_]] with an [[Iso_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    final override def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab)(ev1))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    final override def viewOrModify(s: S): Either[T, C] = self.viewOrModify(s).map(other.view)
  }

  /** compose this [[AffineTraversal_]] with an [[Iso_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): AffineTraversal_[C, D, A, B] = new AffineTraversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[C, D] = other(self(pab))(ev1)

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(c: C): Either[D, A] = self.viewOrModify(other.view(c)).leftMap(other.review)
  }

  /** compose this [[AffineTraversal_]] with an [[AnIso_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    final override def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] =
      self(ev1.dimap(pab)(other.view)(other.review))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    final override def viewOrModify(s: S): Either[T, C] = self.viewOrModify(s).map(other.view)
  }

  /** compose this [[AffineTraversal_]] with an [[AnIso_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): AffineTraversal_[C, D, A, B] = new AffineTraversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[C, D] =
      ev1.dimap(self(pab))(other.view)(other.review)

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(c: C): Either[D, A] = self.viewOrModify(other.view(c)).leftMap(other.review)
  }

  /** compose this [[AffineTraversal_]] with a [[Lens_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    final override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    final override def viewOrModify(s: S): Either[T, C] = self.viewOrModify(s).map(other.view)
  }

  /** compose this [[AffineTraversal_]] with a [[Lens_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): AffineTraversal_[C, D, A, B] = new AffineTraversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[C, D] = other(self(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(c: C): Either[D, A] = self.viewOrModify(other.view(c)).leftMap(other.set(_)(c))
  }

  /** compose this [[AffineTraversal_]] with an [[ALens_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AffineTraversal_[S, T, C, D] =
    AffineTraversal_((s: S) => self.viewOrModify(s).map(other.view))(s => d => self.over(other.set(d))(s))

  /** compose this [[AffineTraversal_]] with an [[ALens_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): AffineTraversal_[C, D, A, B] =
    AffineTraversal_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.set(_)(c)))(c => b => other.over(self.set(b))(c))

  /** compose this [[AffineTraversal_]] with a [[Prism_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    final override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    final override def viewOrModify(s: S): Either[T, C] =
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
  }

  /** compose this [[AffineTraversal_]] with a [[Prism_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): AffineTraversal_[C, D, A, B] = new AffineTraversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[C, D] = other(self(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(c: C): Either[D, A] =
      other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c)))
  }

  /** compose this [[AffineTraversal_]] with an [[APrism_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AffineTraversal_[S, T, C, D] =
    AffineTraversal_((s: S) => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))))(s => d => self.over(other.set(d))(s))

  /** compose this [[AffineTraversal_]] with an [[APrism_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): AffineTraversal_[C, D, A, B] =
    AffineTraversal_((c: C) => other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c))))(c => b => other.over(self.set(b))(c))

  /** compose this [[AffineTraversal_]] with an [[APrism_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    final override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    final override def viewOrModify(s: S): Either[T, C] = self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
  }

  /** compose this [[AffineTraversal_]] with an [[APrism_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): AffineTraversal_[C, D, A, B] = new AffineTraversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[C, D] =
      other(self(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(c: C): Either[D, A] =
      other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c)))
  }

  /** compose this [[AffineTraversal_]] with an [[AnAffineTraversal_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] =
    AffineTraversal_((s: S) => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))))(s => d => self.over(other.set(d))(s))

  /** compose this [[AffineTraversal_]] with an [[AnAffineTraversal_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): AffineTraversal_[C, D, A, B] =
    AffineTraversal_((c: C) => other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c))))(c => b => other.over(self.set(b))(c))

  /** compose this [[AffineTraversal_]] with a [[Traversal_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    final override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose this [[AffineTraversal_]] with a [[Traversal_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): Traversal_[C, D, A, B] = new Traversal_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[C, D] = other(self(pab))
  }

  /** compose this [[AffineTraversal_]] with an [[ATraversal_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      final override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[AffineTraversal_]] with an [[ATraversal_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[AffineTraversal_]] with a [[Setter_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    final override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose this [[AffineTraversal_]] with a [[Setter_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: Setter_[C, D, S, T]): Setter_[C, D, A, B] = new Setter_[C, D, A, B] {
    override private[proptics] def apply(pab: A => B): C => D = other(self(pab))
  }

  /** compose this [[AffineTraversal_]] with a [[Getter_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose this [[AffineTraversal_]] with a [[Getter_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[AffineTraversal_]] with a [[Fold_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    final override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      self[Forget[R, *, *]](other(forget))
  }

  /** compose this [[AffineTraversal_]] with a [[Fold_]], having this [[AffineTraversal_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[AffineTraversal_]] with an [[IndexedLens_]], having this [[AffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      final override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[AffineTraversal_]] with an [[IndexedLens_]], having this [[AffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[AffineTraversal_]] with an [[AnIndexedLens_]], having this [[AffineTraversal_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      final override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[AffineTraversal_]] with an [[AnIndexedLens_]], having this [[AffineTraversal_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[AffineTraversal_]] with an [[IndexedTraversal_]], having this [[AffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      final override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[AffineTraversal_]] with an [[IndexedTraversal_]], having this [[AffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[AffineTraversal_]] with an [[IndexedSetter_]], having this [[AffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    final override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose this [[AffineTraversal_]] with an [[IndexedSetter_]], having this [[AffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedSetter_[I, C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[Function, I, A, B]): C => D =
      other.over { case (s, i) => self.over(a => indexed.runIndex((a, i)))(s) }
  }

  /** compose this [[AffineTraversal_]] with an [[IndexedFold_]], having this [[AffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    final override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[AffineTraversal_]] with an [[IndexedFold_]], having this [[AffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[AffineTraversal_]] with an [[IndexedFold_]], having this [[AffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    final override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[AffineTraversal_]] with an [[IndexedFold_]], having this [[AffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) =>
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      })
  }

  override protected[proptics] def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object AffineTraversal_ {
  /** create a polymorphic [[AffineTraversal_]] from a getter/setter pair */
  final def apply[S, T, A, B](_viewOrModify: S => Either[T, A])(_set: S => B => T): AffineTraversal_[S, T, A, B] =
    AffineTraversal_.traversal((_viewOrModify, _set).mapN(Tuple2.apply))

  /** create a polymorphic [[AffineTraversal_]] from a combined getter/setter */
  final def traversal[S, T, A, B](combined: S => (Either[T, A], B => T)): AffineTraversal_[S, T, A, B] = new AffineTraversal_[S, T, A, B] {
    final override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = {
      val eitherPab = ev1.first[Either[T, A], Either[T, B], B => T](ev0.right(pab))

      ev0.dimap(eitherPab)(combined) { case (f, b) => f.fold(identity, b) }
    }

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    final override def viewOrModify(s: S): Either[T, A] = combined(s)._1
  }

  /** polymorphic identity of an [[AffineTraversal_]] */
  final def id[S, T]: AffineTraversal_[S, T, S, T] = AffineTraversal_[S, T, S, T](_.asRight[T])(const(identity[T]))
}

object AffineTraversal {
  /** create a monomorphic [[AffineTraversal]], using preview and setter functions */
  final def fromPreview[S, A](preview: S => Option[A])(set: S => A => S): AffineTraversal[S, A] =
    AffineTraversal((s: S) => preview(s).fold(s.asLeft[A])(_.asRight[S]))(set)

  /** create a monomorphic [[APrism]], using a partial function and a setter function */
  final def fromPartial[S, A](preview: PartialFunction[S, A])(set: S => A => S): AffineTraversal[S, A] = fromPreview(preview.lift)(set)

  /** create a monomorphic [[AffineTraversal]] from a getter and setter functions */
  final def apply[S, A](viewOrModify: S => Either[S, A])(set: S => A => S): AffineTraversal[S, A] = AffineTraversal_(viewOrModify)(set)

  /** create a monomorphic [[AffineTraversal]] from a pair of getter, setter functions */
  final def traversal[S, A](to: S => (Either[S, A], A => S)): AffineTraversal[S, A] = AffineTraversal_.traversal(to)

  /** monomorphic identity of an [[AffineTraversal]] */
  final def id[S]: AffineTraversal[S, S] = AffineTraversal_.id[S, S]
}
