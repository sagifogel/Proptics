package proptics

import scala.Function.const

import cats.arrow.{Profunctor, Strong}
import cats.data.State
import cats.syntax.bifunctor._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Functor, Monoid}

import proptics.internal._
import proptics.profunctor.{Choice, Closed, Wander}
import proptics.rank2types.{LensLike, LensLikeWithIndex}

/** An [[AnIso_]] is a generalized isomorphism.
  *
  * An [[AnIso_]] is a complete reversible transformation between two types.
  *
  * An [[AnIso_]] is an [[Iso_]] with fixed type [[proptics.internal.Exchange]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of a [[AnIso_]]
  * @tparam T the modified source of an [[AnIso_]]
  * @tparam A the focus of an [[AnIso_]]
  * @tparam B the modified focus of an [[AnIso_]]
  */
abstract class AnIso_[S, T, A, B] { self =>
  private[proptics] def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, S, T]

  /** view the focus of an [[AnIso_]] */
  final def view(s: S): A = toExchange.view(s)

  /** view the modified source of an [[AnIso_]] */
  def review(b: B): T

  /** set the modified focus of an [[AnIso_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[AnIso_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = s => self.review(f(toExchange.view(s)))

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AnIso_]] using a Functor, resulting in a change of type to the full structure */
  final def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] = ev.map(f(view(s)))(set(_)(s))

  /** test whether a predicate holds for the focus of an [[AnIso_]] */
  final def exists(f: A => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of an [[AnIso_]] */
  final def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus contains a given value */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus does not contain a given value */
  final def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** find if the focus of an [[AnIso_]] is satisfying a predicate. */
  final def find(f: A => Boolean): S => Option[A] = s => view(s).some.filter(f)

  /** convert an [[AnIso_]] to the pair of functions that characterize it */
  final def withIso[R](f: (S => A) => (B => T) => R): R = {
    val exchange: Exchange[A, B, S, T] = toExchange

    f(exchange.view)(exchange.review)
  }

  /** convert an [[AnIso_]] to an Exchange[A, B, S, T] */
  final def toExchange: Exchange[A, B, S, T] = self(Exchange(identity, identity))

  /** view the focus of a [[Lens_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, A] = ev.inspect(view)

  /** modify an effectful focus of an [[AnIso_]] to the type of the modified focus, resulting in a change of type to the full structure */
  final def cotraverse[F[_]](fs: F[S])(f: F[A] => B)(implicit ev: Applicative[F]): T = {
    val exchange: Exchange[A, B, S, T] = toExchange

    exchange.review(f(ev.map(fs)(exchange.view)))
  }

  /** synonym for [[cotraverse]], flipped */
  final def zipWithF[F[_]: Applicative](f: F[A] => B)(fs: F[S]): T = cotraverse(fs)(f)

  /** the opposite of working over a [[AnIso_.set]] is working under an isomorphism */
  final def under(f: T => S): B => A = withIso(sa => bt => sa compose f compose bt)

  /** lift an [[Iso_]] into an arbitrary Functor. */
  final def mapping[F[_], G[_]](implicit ev0: Functor[F], ev1: Functor[G]): Iso_[F[S], G[T], F[A], G[B]] =
    withIso(sa => bt => Iso_(ev0.lift(sa))(ev1.lift(bt)))

  /** lift two [[Iso_]] instances into both arguments of a [[cats.arrow.Profunctor]] simultaneously. */
  final def dimapping[P[_, _], Q[_, _], SS, TT, AA, BB](
      other: AnIso_[SS, TT, AA, BB])(implicit ev0: Profunctor[P], ev1: Profunctor[Q]): Iso_[P[A, SS], Q[B, TT], P[S, AA], Q[T, BB]] =
    withIso[Iso_[P[A, SS], Q[B, TT], P[S, AA], Q[T, BB]]] { sa => bt =>
      other.withIso[Iso_[P[A, SS], Q[B, TT], P[S, AA], Q[T, BB]]] { ssaa => bbtt =>
        Iso_.iso[P[A, SS], Q[B, TT], P[S, AA], Q[T, BB]](ev0.dimap(_)(sa)(ssaa))(ev1.dimap(_)(bt)(bbtt))
      }
    }

  /** reverse an [[AnIso_]] by swapping the source and the focus */
  final def reverse: AnIso_[B, A, T, S] = new AnIso_[B, A, T, S] {
    override private[proptics] def apply(exchange: Exchange[T, S, T, S]): Exchange[T, S, B, A] = Exchange(self.review, self.view)

    override def review(s: S): A = self.view(s)
  }

  /** transform an [[AnIso_]] to an [[Iso_]] */
  final def asIso: Iso_[S, T, A, B] = self.withIso(Iso_[S, T, A, B])

  /** compose this [[AnIso_]] with a function lifted to a [[Getter_]], having this [[AnIso_]] applied last */
  final def to[C, D](f: A => C): Getter_[S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose this [[AnIso_]] with an [[Iso_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Iso_[A, B, C, D]): AnIso_[S, T, C, D] = new AnIso_[S, T, C, D] {
    override private[proptics] def apply(exchange: Exchange[C, D, C, D]): Exchange[C, D, S, T] =
      self.toExchange compose other(exchange)

    override def review(d: D): T = self.review(other.review(d))
  }

  /** compose this [[AnIso_]] with an [[Iso_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Iso_[C, D, S, T]): AnIso_[C, D, A, B] = new AnIso_[C, D, A, B] {
    override private[proptics] def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, C, D] =
      Exchange(exchange.view compose self.view compose other.view, other.review _ compose self.review compose exchange.review)

    /** view the modified source of an [[AnIso_]] */
    override def review(b: B): D = other.review(self.review(b))
  }

  /** compose this [[AnIso_]] with an [[AnIso_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: AnIso_[A, B, C, D]): AnIso_[S, T, C, D] = new AnIso_[S, T, C, D] {
    override private[proptics] def apply(exchange: Exchange[C, D, C, D]): Exchange[C, D, S, T] =
      self.toExchange compose other(exchange)

    override def review(d: D): T = self.review(other.review(d))
  }

  /** compose this [[AnIso_]] with an [[AnIso_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: AnIso_[C, D, S, T]): AnIso_[C, D, A, B] = new AnIso_[C, D, A, B] {
    override private[proptics] def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, C, D] =
      Exchange(exchange.view compose self.view compose other.view, other.review _ compose self.review compose exchange.review)

    /** view the modified source of an [[AnIso_]] */
    override def review(b: B): D = other.review(self.review(b))
  }

  /** compose this [[AnIso_]] with a [[Lens_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Lens_[A, B, C, D]): Lens_[S, T, C, D] = new Lens_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Strong[P]): P[S, T] =
      ev.dimap(other(pab))(self.view)(self.review)
  }

  /** compose this [[AnIso_]] with a [[Lens_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Lens_[C, D, S, T]): Lens_[C, D, A, B] =
    Lens_(self.view _ compose other.view)(c => b => other.set(self.review(b))(c))

  /** compose this [[AnIso_]] with an [[ALens_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: ALens_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] =
      Shop(shop.view compose other.view compose self.view, s => d => self.over(other(shop).set(_)(d))(s))
  }

  /** compose this [[AnIso_]] with an [[ALens_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: ALens_[C, D, S, T]): ALens_[C, D, A, B] =
    ALens_(self.view _ compose other.view)(c => b => other.set(self.review(b))(c))

  /** compose this [[AnIso_]] with a [[Prism_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Prism_[A, B, C, D]): Prism_[S, T, C, D] = new Prism_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Choice[P]): P[S, T] =
      ev.dimap(other(pab))(self.view)(self.review)

    /** view the focus of a [[Prism_]] or return the modified source of a [[Prism_]] */
    override def viewOrModify(s: S): Either[T, C] = other.viewOrModify(self.view(s)).leftMap(self.set(_)(s))
  }

  /** compose this [[AnIso_]] with a [[Prism_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Prism_[C, D, S, T]): Prism_[C, D, A, B] =
    Prism_((c: C) => other.viewOrModify(c).map(self.view))(other.review _ compose self.review)

  /** compose this [[AnIso_]] with an [[APrism_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: APrism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] = {
      val exchange: Exchange[A, B, S, T] = toExchange
      val marketFromExchange = Market(Right[T, A] _ compose exchange.view, exchange.review)

      marketFromExchange compose other(market)
    }

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.overF(f))
  }

  /** compose this [[AnIso_]] with an [[APrism_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: APrism_[C, D, S, T]): APrism_[C, D, A, B] =
    APrism_((c: C) => other.viewOrModify(c).map(self.view))(other.review _ compose self.review)

  /** compose this [[AnIso_]] with an [[AffineTraversal_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] =
      ev0.dimap(other(pab))(self.view)(self.review)

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, C] = other.viewOrModify(self.view(s)).leftMap(review)
  }

  /** compose this [[AnIso_]] with an [[AffineTraversal_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[C, D, S, T]): AffineTraversal_[C, D, A, B] =
    AffineTraversal_((c: C) => other.viewOrModify(c).map(self.view))(c => b => other.set(self.review(b))(c))

  /** compose this [[AnIso_]] with an [[AnAffineTraversal_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] = new AnAffineTraversal_[S, T, C, D] {
    override private[proptics] def apply(pab: Stall[C, D, C, D]): Stall[C, D, S, T] =
      Stall(s => other.viewOrModify(self.view(s)).leftMap(self.review), s => d => self.set(other.set(d)(self.view(s)))(s))

    /** view the focus of an [[AnAffineTraversal_]] or return the modified source of an [[AnAffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, C] = other.viewOrModify(self.view(s)).leftMap(review)
  }

  /** compose this [[AnIso_]] with an [[AnAffineTraversal_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => other.viewOrModify(c).map(self.view))(c => b => other.set(self.review(b))(c))

  /** compose this [[AnIso_]] with a [[Traversal_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] =
      ev.dimap(other(pab))(self.view)(self.review)
  }

  /** compose this [[AnIso_]] with a [[Traversal_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Traversal_[C, D, S, T]): Traversal_[C, D, A, B] =
    Traversal_.wander(new LensLike[C, D, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): C => F[D] = other.overF(self.overF(f))
    })

  /** compose this [[AnIso_]] with an [[ATraversal_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[AnIso_]] with an [[ATraversal_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: ATraversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[AnIso_]] with a [[Setter_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self.over(other(pab))
  }

  /** compose this [[AnIso_]] with a [[Setter_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Setter_[C, D, S, T]): Setter_[C, D, A, B] = new Setter_[C, D, A, B] {
    override private[proptics] def apply(pab: A => B): C => D = other(self.over(pab))
  }

  /** compose this [[AnIso_]] with a [[Getter_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  /** compose this [[AnIso_]] with a [[Getter_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Getter_[C, D, S, T]): Getter_[C, D, A, B] = new Getter_[C, D, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, C, D] =
      Forget(self.view _ compose other.view)
  }

  /** compose this [[AnIso_]] with a [[Fold_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(forget.runForget))
  }

  /** compose this [[AnIso_]] with a [[Fold_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(forget.runForget compose self.view))
  }

  /** compose this [[AnIso_]] with a [[Grate_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Grate_[A, B, C, D]): Grate_[S, T, C, D] = new Grate_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Closed[P]): P[S, T] =
      ev.dimap(other(pab))(self.view)(self.review)
  }

  /** compose this [[AnIso_]] with a [[Grate_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Grate_[C, D, S, T]): Grate_[C, D, A, B] = new Grate_[C, D, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[C, D] =
      other(ev.dimap(pab)(self.view)(self.review))
  }

  /** compose this [[AnIso_]] with a [[Review_]], having this [[AnIso_]] applied last */
  final def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] =
      Tagged(self.review(other.review(tagged.runTag)))
  }

  /** compose this [[AnIso_]] with a [[Review_]], having this [[AnIso_]] applied first */
  final def andThen[C, D](other: Review_[C, D, S, T]): Review_[C, D, A, B] = new Review_[C, D, A, B] {
    override private[proptics] def apply(tagged: Tagged[A, B]): Tagged[C, D] =
      Tagged(other.review(self.review(tagged.runTag)))
  }

  /** compose this [[AnIso_]] with an [[IndexedLens_]], having this [[AnIso_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedLens_[I, S, T, C, D] =
    IndexedLens_((s: S) => other.view(self.view(s)))(s => d => self.set(other.set(d)(self.view(s)))(s))

  /** compose this [[AnIso_]] with an [[IndexedLens_]], having this [[AnIso_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedLens_[I, C, D, A, B] =
    IndexedLens_((c: C) => other.view(c).leftMap(self.view))(c => b => other.over { case (s, _) => self.set(b)(s) }(c))

  /** compose this [[AnIso_]] with an [[AnIndexedLens_]], having this [[AnIso_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_((s: S) => other.view(self.view(s)))(s => d => self.set(other.set(d)(self.view(s)))(s))

  /** compose this [[AnIso_]] with an [[AnIndexedLens_]], having this [[AnIso_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): AnIndexedLens_[I, C, D, A, B] =
    AnIndexedLens_((c: C) => other.view(c).leftMap(self.view))(c => b => other.over { case (s, _) => self.set(b)(s) }(c))

  /** compose this [[AnIso_]] with an [[IndexedTraversal_]], having this [[AnIso_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[AnIso_]] with an [[IndexedTraversal_]], having this [[AnIso_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[AnIso_]] with an [[IndexedSetter_]], having this [[AnIso_]] applied last */
  final def compose[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose this [[AnIso_]] with an [[IndexedSetter_]], having this [[AnIso_]] applied first */
  final def andThen[I, C, D](other: IndexedSetter_[I, C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[Function, I, A, B]): C => D =
      other.over { case (s, i) => self.over(a => indexed.runIndex((a, i)))(s) }
  }

  /** compose this [[AnIso_]] with an [[IndexedGetter_]], having this [[AnIso_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose other.view compose self.view)
  }

  /** compose this [[AnIso_]] with an [[IndexedGetter_]], having this [[AnIso_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => indexed.runIndex.runForget(other.view(c).leftMap(self.view)))
  }

  /** compose this [[AnIso_]] with an [[IndexedFold_]], having this [[AnIso_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(indexed.runIndex.runForget))
  }

  /** compose this [[AnIso_]] with an [[IndexedFold_]], having this [[AnIso_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => indexed.runIndex.runForget((self.view(s), i)) })
  }
}

object AnIso_ {
  /** create a polymorphic [[AnIso_]] from an Iso encoded in Exchange */
  private[proptics] def apply[S, T, A, B](f: Exchange[A, B, A, B] => Exchange[A, B, S, T]): AnIso_[S, T, A, B] = new AnIso_[S, T, A, B] { self =>
    override def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, S, T] = f(exchange)

    override def review(b: B): T = f(Exchange(identity, identity)).review(b)
  }

  /** create a polymorphic [[AnIso_]] from view/review pair */
  final def apply[S, T, A, B](view: S => A)(review: B => T): AnIso_[S, T, A, B] =
    AnIso_((ex: Exchange[A, B, A, B]) => Exchange(ex.view compose view, review compose ex.review))

  /** polymorphic identity of an [[AnIso_]] */
  final def id[S, T]: AnIso_[S, T, S, T] = AnIso_(identity[S] _)(identity[T])
}

object AnIso {
  /** create a monomorphic [[AnIso]] from view/review pair */
  final def apply[S, A](view: S => A)(review: A => S): AnIso[S, A] = AnIso_(view)(review)

  /** monomorphic identity of an [[AnIso]] */
  final def id[S]: AnIso[S, S] = AnIso_.id[S, S]
}
