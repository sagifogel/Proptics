package proptics

import scala.Function.const

import cats.arrow.Strong
import cats.data.State
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Applicative, Comonad, Eq, Functor, Monoid}

import proptics.data.Disj
import proptics.internal._
import proptics.profunctor.{Choice, Costar, Star, Wander}
import proptics.rank2types.{LensLikeWithIndex, Rank2TypeLensLike}
import proptics.syntax.costar._
import proptics.syntax.star._

/** Given a type whose "focus element" always exists,
  * a [[Lens_]] provides a convenient way to view, set, and transform
  * that element.
  *
  * @tparam S the source of a [[Lens_]]
  * @tparam T the modified source of a [[Lens_]]
  * @tparam A the focus of a [[Lens_]]
  * @tparam B the modified focus of a [[Lens_]]
  */
abstract class Lens_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T]

  /** view the focus of a [[Lens_]] */
  final def view(s: S): A = self[Forget[A, *, *]](Forget(identity)).runForget(s)

  /** set the modified focus of a [[Lens_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the focus type of a [[Lens_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = self(f)

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Functor](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of a [[Lens_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  final def traverse[F[_]: Functor](s: S)(f: A => F[B]): F[T] = self(Star(f)).runStar(s)

  /** test whether a predicate holds for the focus of a [[Lens_]] */
  final def exists(f: A => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of a [[Lens_]] */
  final def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of a [[Lens_]] contains a given value */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus a [[Lens_]] does not contain a given value */
  final def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** find if the focus of a [[Lens_]] is satisfying a predicate. */
  final def find(f: A => Boolean): S => Option[A] = s => view(s).some.filter(f)

  /** view the focus of a [[Lens_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, A] = ev.inspect(view)

  /** try to map a function over this [[Lens_]], failing if the [[Lens_]] has no focus. */
  final def failover[F[_]](f: A => B)(s: S)(implicit ev0: Strong[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] = {
    val star = Star[(Disj[Boolean], *), A, B](a => (Disj(true), f(a)))

    self(star).runStar(s) match {
      case (Disj(true), x) => ev1.pure(x)
      case (Disj(false), _) => ev1.empty
    }
  }

  /** zip two sources of a [[Lens_]] together provided a binary operation which modify the focus type of a [[Lens_]] */
  final def zipWith[F[_]](s1: S, s2: S)(f: (A, A) => B): T = self(Zipping(f.curried)).runZipping(s1)(s2)

  /** modify an effectual focus of an [[Lens_]] into the modified focus, resulting in a change of type to the full structure */
  final def cotraverse[F[_]: Comonad](fs: F[S])(f: F[A] => B)(implicit ev: Applicative[F]): T = self(Costar(f)).runCostar(fs)

  /** synonym for [[cotraverse]], flipped */
  final def zipWithF[F[_]: Comonad](f: F[A] => B)(fs: F[S]): T = self(Costar(f)).runCostar(fs)

  /** transform a [[Lens_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(forget.runForget compose self.view)
  }

  /** compose a [[Lens_]] with a function lifted to a [[Getter_]] */
  final def to[C, D](f: A => C): Getter_[S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose a [[Lens_]] with an [[Iso_]] */
  final def compose[C, D](other: Iso_[A, B, C, D]): Lens_[S, T, C, D] = new Lens_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Strong[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Lens_]] with an [[AnIso_]] */
  final def compose[C, D](other: AnIso_[A, B, C, D]): Lens_[S, T, C, D] = self compose other.asIso

  /** compose a [[Lens_]] with a [[Lens_]] */
  final def compose[C, D](other: Lens_[A, B, C, D]): Lens_[S, T, C, D] = new Lens_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Strong[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Lens_]] with an [[ALens_]] */
  final def compose[C, D](other: ALens_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] = self(other(shop))
  }

  /** compose a [[Lens_]] with a [[Prism_]] */
  final def compose[C, D](other: Prism_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, C] = other.viewOrModify(self.view(s)).leftMap(self.set(_)(s))
  }

  /** compose a [[Lens_]] with an [[APrism_]] */
  final def compose[C, D](other: APrism_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self compose other.asPrism

  /** compose a [[Lens_]] with a [[AffineTraversal_]] */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, C] = other.viewOrModify(self.view(s)).leftMap(self.set(_)(s))
  }

  /** compose [[Lens_]] with an [[AnAffineTraversal_]] */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      other.viewOrModify(self.view(s)).leftMap(self.set(_)(s))
    }(s => d => self.over(other.set(d))(s))

  /** compose a [[Lens_]] with a [[Traversal_]] */
  final def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Lens_]] with an [[ATraversal_]] */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] = ATraversal_(new RunBazaar[* => *, C, D, S, T] {
    override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(pafb))
  })

  /** compose a [[Lens_]] with a [[Setter_]] */
  final def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Lens_]] with a [[Getter_]] */
  final def compose[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] = self(other(Forget(identity)))
  }

  /** compose a [[Lens_]] with a [[Fold_]] */
  final def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose a [[Lens_]] with an [[IndexedLens_]] */
  final def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedLens_[I, S, T, C, D] =
    IndexedLens_[I, S, T, C, D]((s: S) => other.view(self.view(s)))(s => d => self.set(other.set(d)(self.view(s)))(s))

  /** compose a [[Lens_]] with an [[AnIndexedLens_]] */
  final def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_[I, S, T, C, D]((s: S) => other.view(self.view(s)))(s => d => self.set(other.set(d)(self.view(s)))(s))

  /** compose a [[Lens_]] with an [[IndexedTraversal_]] */
  final def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose a [[Lens_]] with an [[IndexedSetter_]] */
  final def compose[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T = s => self.set(other.over { case (c, i) => indexed.runIndex((c, i)) }(self.view(s)))(s)
  }

  /** compose a [[Lens_]] with an [[IndexedGetter_]] */
  final def compose[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose other.view compose self.view)
  }

  /** compose a [[Lens_]] with an [[IndexedFold_]] */
  final def compose[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(indexed.runIndex.runForget))
  }
}

object Lens_ {
  /** create a polymorphic [[Lens_]] from Rank2TypeLensLike encoding */
  private[proptics] def apply[S, T, A, B](f: Rank2TypeLensLike[S, T, A, B]): Lens_[S, T, A, B] = new Lens_[S, T, A, B] { self =>
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] = f(pab)
  }

  /** create a polymorphic [[Lens_]] from a getter/setter pair */
  final def apply[S, T, A, B](view: S => A)(set: S => B => T): Lens_[S, T, A, B] = Lens_.lens((view, set).mapN(Tuple2.apply))

  /** create a polymorphic [[Lens_]] from a combined getter/setter */
  final def lens[S, T, A, B](to: S => (A, B => T)): Lens_[S, T, A, B] = Lens_(new Rank2TypeLensLike[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] = liftOptic(to)(ev)(pab)
  })

  /** polymorphic identity of a [[Lens_]] */
  final def id[S, T]: Lens_[S, T, S, T] = Lens_[S, T, S, T](identity[S] _)(const(identity))

  /** use a [[Prism_]] as a kind of first-class pattern. */
  final def outside[S, T, A, B, R](prism: Prism_[S, T, A, B]): Lens_[T => R, S => R, B => R, A => R] =
    Lens_[T => R, S => R, B => R, A => R]((f: T => R) => f compose prism.review) { t2r => a2r => s =>
      prism.viewOrModify(s).fold(t2r, a2r)
    }

  /** lift a combined getter/setter function to a general optic using [[Strong]] profunctor */
  private[proptics] def liftOptic[P[_, _], S, T, A, B](to: S => (A, B => T))(implicit ev: Strong[P]): P[A, B] => P[S, T] =
    pab => ev.dimap(ev.first[A, B, B => T](pab))(to) { case (b, f) => f(b) }

  /** implicit conversion from [[APrism_]] to [[Prism_]] */
  implicit def aLensToLens[S, T, A, B](aLens: ALens_[S, T, A, B]): Lens_[S, T, A, B] = aLens.asLens
}

object Lens {
  /** create a monomorphic [[Lens]] from a getter/setter pair */
  final def apply[S, A](view: S => A)(set: S => A => S): Lens[S, A] = Lens_[S, S, A, A](view)(set)

  /** create a monomorphic [[Lens]] from a combined getter/setter function */
  final def lens[S, A](to: S => (A, A => S)): Lens[S, A] = Lens_.lens[S, S, A, A](to)

  /** monomorphic identity of a [[Lens]] */
  final def id[S]: Lens[S, S] = Lens_.id[S, S]

  /** use a [[Prism]] as a kind of first-class pattern. */
  final def outside[S, A, R](aPrism: Prism[S, A]): Lens[S => R, A => R] = Lens_.outside(aPrism)
}
