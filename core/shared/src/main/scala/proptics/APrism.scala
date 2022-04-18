package proptics

import scala.Function.const

import cats.syntax.either._
import cats.syntax.eq._
import cats.{Alternative, Applicative, Eq, Id, Monoid}

import proptics.data.Disj
import proptics.internal._
import proptics.profunctor.{Choice, Star}
import proptics.rank2types.{LensLike, LensLikeWithIndex}

/** [[APrism_]] is used for selecting cases of a type, most often a sum type.
  *
  * [[APrism_]] is a [[Prism_]] with fixed type [[proptics.internal.Market]] [[cats.arrow.Profunctor]]
  *
  * @tparam S
  *   the source of an [[APrism_]]
  * @tparam T
  *   the modified source of an [[APrism_]]
  * @tparam A
  *   the focus of an [[APrism_]]
  * @tparam B
  *   the modified focus of an [[APrism_]]
  */
abstract class APrism_[S, T, A, B] extends Prism0[S, T, A, B] { self =>
  private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T]

  /** view the focus of an [[APrism_]] or return the modified source of an [[APrism_]] */
  final def viewOrModify(s: S): Either[T, A] = withPrism(matching => const(matching(s)))

  /** view the modified source of an [[APrism_]] */
  final def review(b: B): T = toMarket.review(b)

  /** modify the focus type of an [[APrism_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = overF[Id](f)

  /** modify the focus type of an [[APrism_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T]

  /** try to map a function over this [[APrism_]], failing if the [[APrism_]] has no focus. */
  final def failover[F[_]](f: A => B)(s: S)(implicit ev0: Choice[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] =
    asPrism.failover(f)(s)

  /** convert an [[APrism_]] to the pair of functions that characterize it */
  final def withPrism[R](f: (S => Either[T, A]) => (B => T) => R): R = {
    val market = toMarket

    f(market.viewOrModify)(market.review)
  }

  /** convert an [[APrism_]] to an Market[A, B, S, T] */
  final def toMarket: Market[A, B, S, T] = self(Market(_.asRight[B], identity[B]))

  /** transform an [[APrism_]] to a [[Prism_]] */
  final def asPrism: Prism_[S, T, A, B] = withPrism(Prism_[S, T, A, B])

  /** transform an [[APrism_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget))
  }

  /** compose this [[APrism_]] with a function lifted to a [[Getter_]], having this [[APrism_]] applied first */
  final def focus[C, D](f: A => C): Fold_[S, T, C, D] = andThen(Getter_[A, B, C, D](f))

  /** compose this [[APrism_]] with an [[Iso_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): APrism_[S, T, C, D] =
    APrism_((s: S) => self.viewOrModify(s).map(other.view))(self.review _ compose other.review)

  /** compose this [[APrism_]] with an [[Iso_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): APrism_[C, D, A, B] =
    APrism_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.review))(other.review _ compose self.review)

  /** compose this [[APrism_]] with an [[AnIso_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): APrism_[S, T, C, D] =
    APrism_((s: S) => self.viewOrModify(s).map(other.view))(self.review _ compose other.review)

  /** compose this [[APrism_]] with an [[AnIso_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): APrism_[C, D, A, B] =
    APrism_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.review))(other.review _ compose self.review)

  /** compose this [[APrism_]] with a [[Lens_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AffineTraversal_[S, T, C, D] =
    AffineTraversal_((s: S) => self.viewOrModify(s).map(other.view))(s => d => self.over(other.set(d)(_))(s))

  /** compose this [[APrism_]] with a [[Lens_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): AffineTraversal_[C, D, A, B] =
    AffineTraversal_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.set(_)(c)))(c => b => other.set(self.review(b))(c))

  /** compose this [[APrism_]] with an [[ALens_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AffineTraversal_[S, T, C, D] =
    AffineTraversal_((s: S) => self.viewOrModify(s).map(other.view))(s => d => self.over(other.set(d)(_))(s))

  /** compose this [[APrism_]] with an [[ALens_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): AffineTraversal_[C, D, A, B] =
    AffineTraversal_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.set(_)(c)))(c => b => other.set(self.review(b))(c))

  /** compose this [[APrism_]] with a [[Prism_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] =
      self.toMarket andThen other(market)

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.overF(f))
  }

  /** compose this [[APrism_]] with a [[Prism_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): APrism_[C, D, A, B] = new APrism_[C, D, A, B] {
    override private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, C, D] =
      Market(other.viewOrModify(_).flatMap(self.viewOrModify(_).leftMap(other.review)), other.review _ compose self.review)

    /** modify the focus type of an [[APrism_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
    override def traverse[F[_]](s: C)(f: A => F[B])(implicit ev: Applicative[F]): F[D] =
      other.traverse(s)(self.overF(f))
  }

  /** compose this [[APrism_]] with an [[APrism_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] = self.toMarket andThen other(market)

    /** modify the focus type of an [[APrism_]] using a Functor, resulting in a change of type to the full structure */
    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] = self.traverse(s)(other.overF(f))
  }

  /** compose this [[APrism_]] with an [[APrism_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): APrism_[C, D, A, B] = new APrism_[C, D, A, B] {
    override private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, C, D] = other.toMarket andThen self(market)

    /** modify the focus type of an [[APrism_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
    override def traverse[F[_]](s: C)(f: A => F[B])(implicit ev: Applicative[F]): F[D] = other.traverse(s)(self.overF(f))
  }

  /** compose this [[APrism_]] with an [[AffineTraversal_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] =
    AffineTraversal_((s: S) => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))))(s => d => self.over(other.set(d))(s))

  /** compose this [[APrism_]] with an [[AffineTraversal_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): AffineTraversal_[C, D, A, B] =
    AffineTraversal_((c: C) => other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c))))(c => b => other.over(self.set(b))(c))

  /** compose this [[APrism_]] with an [[AnAffineTraversal_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))))(s => d => self.over(other.set(d))(s))

  /** compose this [[APrism_]] with an [[AnAffineTraversal_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c))))(c => b => other.over(self.set(b))(c))

  /** compose this [[APrism_]] with a [[Traversal_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] =
    Traversal_.wander(new LensLike[S, T, C, D] {
      override def apply[F[_]](f: C => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[APrism_]] with a [[Traversal_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): Traversal_[C, D, A, B] =
    Traversal_.wander(new LensLike[C, D, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF(f))
    })

  /** compose this [[APrism_]] with an [[ATraversal_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(f))
    })

  /** compose this [[APrism_]] with an [[ATraversal_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(s: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(s)(self.overF(pafb))
    })

  /** compose this [[APrism_]] with a [[Setter_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T =
      toMarket.viewOrModify(_).fold(identity, self.review _ compose other(pab))
  }

  /** compose this [[APrism_]] with a [[Setter_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: Setter_[C, D, S, T]): Setter_[C, D, A, B] = new Setter_[C, D, A, B] {
    override private[proptics] def apply(pab: A => B): C => D =
      other.over(s => self.viewOrModify(s).fold(identity, a => self.set(pab(a))(s)))
  }

  /** compose this [[APrism_]] with a [[Getter_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose this [[APrism_]] with a [[Getter_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[APrism_]] with a [[Fold_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose this [[APrism_]] with a [[Fold_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[APrism_]] with a [[Review_]], having this [[APrism_]] applied first */
  final def andThen[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] =
      Tagged(self.review(other(tagged).runTag))
  }

  /** compose this [[APrism_]] with a [[Review_]], having this [[APrism_]] applied last */
  final def compose[C, D](other: Review_[C, D, S, T]): Review_[C, D, A, B] = new Review_[C, D, A, B] {
    override private[proptics] def apply(tagged: Tagged[A, B]): Tagged[C, D] =
      Tagged(other.review(self.review(tagged.runTag)))
  }

  /** compose this n[[APrism_]] with an [[IndexedLens_]], having this [[APrism_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[APrism_]] with an [[IndexedLens_]], having this [[APrism_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[APrism_]] with an [[AnIndexedLens_]], having this [[APrism_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[APrism_]] with an [[AnIndexedLens_]], having this [[APrism_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[APrism_]] with an [[IndexedTraversal_]], having this [[APrism_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[APrism_]] with an [[IndexedTraversal_]], having this [[APrism_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[APrism_]] with an [[IndexedSetter_]], having this [[APrism_]] applied first */
  final def andThen[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose this [[APrism_]] with an [[IndexedSetter_]], having this [[APrism_]] applied last */
  final def compose[I, C, D](other: IndexedSetter_[I, C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over { case (s, i) => self.over(a => indexed.runIndex((a, i)))(s) }
  }

  /** compose this [[APrism_]] with an [[IndexedGetter_]], having this [[APrism_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[APrism_]] with an [[IndexedGetter_]], having this [[APrism_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[APrism_]] with an [[IndexedFold_]], having this [[APrism_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[APrism_]] with an [[IndexedFold_]], having this [[APrism_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => self.foldMap(s)(a => indexed.runIndex.runForget((a, i))) })
  }
}

object APrism_ {
  /** create a polymorphic [[APrism_]] from a matcher function that produces an Either and a review function
    *
    * the matcher function returns an Either to allow for type-changing prisms in the case where the input does not match.
    */
  final def apply[S, T, A, B](_viewOrModify: S => Either[T, A])(_review: B => T): APrism_[S, T, A, B] = new APrism_[S, T, A, B] { self =>
    override private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T] = Market(_viewOrModify, _review)

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] = viewOrModify(s) match {
      case Right(a) => ev.map(f(a))(review)
      case Left(t) => ev.pure(t)
    }
  }

  /** polymorphic identity of an [[APrism_]] */
  final def id[S, T]: APrism_[S, T, S, T] = APrism_[S, T, S, T](_.asRight[T])(identity)
}

object APrism {
  /** create a monomorphic [[APrism]], using preview and review functions */
  final def fromPreview[S, A](preview: S => Option[A])(review: A => S): APrism[S, A] =
    APrism((s: S) => preview(s).fold(s.asLeft[A])(_.asRight[S]))(review)

  /** create a monomorphic [[APrism]], using a partial function and a review function */
  final def fromPartial[S, A](preview: PartialFunction[S, A])(review: A => S): APrism[S, A] = fromPreview(preview.lift)(review)

  /** create a monomorphic [[APrism]] from a matcher function that produces an Either and a review function
    *
    * the matcher function returns an Either to allow for type-changing prisms in the case where the input does not match.
    */
  final def apply[S, A](viewOrModify: S => Either[S, A])(review: A => S): APrism[S, A] = APrism_(viewOrModify)(review)

  /** create a monomorphic [[APrism]] that checks whether the focus matches a predicate */
  final def nearly[A](a: A)(predicate: A => Boolean)(implicit ev: Alternative[Option]): Prism[A, Unit] =
    APrism.fromPreview[A, Unit](ev.guard _ compose predicate)(const(a))

  /** create a monomorphic [[APrism]] that checks whether the focus matches a single value */
  final def only[A: Eq](a: A)(implicit ev: Alternative[Option]): Prism[A, Unit] = nearly(a)(_ === a)

  /** monomorphic identity of an [[APrism]] */
  final def id[S]: APrism[S, S] = APrism_.id[S, S]
}
