package proptics

import scala.Function.const

import cats.data.Const
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid}
import spire.algebra.lattice.Heyting
import spire.std.boolean._

import proptics.internal._
import proptics.newtype.Newtype._
import proptics.newtype.{Conj, Disj, First, Newtype}
import proptics.profunctor.{Traversing, Wander}
import proptics.rank2types.LensLikeWithIndex

/** * [[APrism]] is a [[Prism_]] with fixed type [[Market]] Profunctor
  *
  * @tparam S the source of an [[APrism_]]
  * @tparam T the modified source of an [[APrism_]]
  * @tparam A the focus of an [[APrism_]]
  * @tparam B the modified focus of an [[APrism_]]
  */
abstract class APrism_[S, T, A, B] { self =>
  private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T]

  /** view the focus of an [[APrism_]] or return the modified source of an [[APrism_]] */
  def viewOrModify(s: S): Either[T, A] = withPrism(matching => const(matching(s)))

  /** view an optional focus of an [[APrism_]] */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  /** view the modified source of an [[APrism_]] */
  def review(b: B): T = toMarket.review(b)

  /** set the modified focus of an [[APrism_]] */
  def set(b: B): S => T = over(const(b))

  /** set the focus of an [[APrism_]] conditionally if it is not None */
  def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus type of an [[APrism_]] using a function, resulting in a change of type to the full structure */
  def over(f: A => B): S => T = overF[Id](f)

  /** modify the focus of an [[APrism_]] using a function conditionally if it is not None, resulting in a change of type to the full structure */
  def overOption(f: A => B): S => Option[T] = s => preview(s).map(review _ compose f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[APrism_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T]

  /** test whether there is no focus or a predicate holds for the focus of an [[APrism_]] */
  def forall(p: A => Boolean): S => Boolean = preview(_).forall(p)

  /** test whether there is no focus or a predicate holds for the focus of an [[APrism_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** test whether a predicate holds for the focus of an [[APrism_]] */
  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](_)(f)

  /** test whether a predicate does not hold for the focus of an [[APrism_]] */
  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** tests whether the focus of an [[APrism_]] contains a given value */
  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus of an [[APrism_]] does not contain a given value */
  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** check if the [[APrism_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[APrism_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** find if the focus of an [[APrism_]] is satisfying a predicate. */
  def find(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  /** convert an [[APrism_]] to the pair of functions that characterize it */
  def withPrism[R](f: (S => Either[T, A]) => (B => T) => R): R = {
    val market = toMarket

    f(market.viewOrModify)(market.review)
  }

  /** convert an [[APrism_]] to an Market[A, B, S, T] */
  def toMarket: Market[A, B, S, T] = self(Market(_.asRight[B], identity[B]))

  /** transform an [[APrism_]] to a [[Prism_]] */
  def asPrism: Prism_[S, T, A, B] = withPrism(Prism_[S, T, A, B])

  /** transform an [[APrism_]] to a [[Fold_]] */
  def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.preview(_).fold(Monoid[R].empty)(forget.runForget))
  }

  /** compose an [[APrism_]] with a function lifted to a [[Getter_]] */
  def to[C, D](f: A => C): Fold_[S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose an [[APrism_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] =
      self.toMarket compose other(market)

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  /** compose an [[APrism_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): APrism_[S, T, C, D] = self compose other.asIso

  /** compose an [[APrism_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self.asPrism compose other

  /** compose an [[APrism_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self.asPrism compose other

  /** compose an [[APrism_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] =
      self.toMarket compose other(market)

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  /** compose an [[APrism_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] =
      self.toMarket compose other(market)

    /** modify the focus type of an [[APrism_]] using a Functor, resulting in a change of type to the full structure */
    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] = self.traverse(s)(other.traverse(_)(f))
  }

  /** compose an [[APrism_]] with an [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] =
    AffineTraversal_ { (s: S) =>
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[APrism_]] with an [[AnAffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[APrism_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pcd: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pcd)
    }
  }

  /** compose an [[APrism_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(f))
    })

  /** compose an [[APrism_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T =
      toMarket.viewOrModify(_).fold(identity, self.review _ compose other(pab))
  }

  /** compose an [[APrism_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose an [[APrism_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose an [[APrism_]] with a [[Review_]] */
  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = self.asPrism compose other

  /** compose a n[[APrism_]] with an [[IndexedLens_]] */
  def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose a n[[APrism_]] with an [[AnIndexedLens_]] */
  def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose an [[APrism_]] with an [[IndexedTraversal_]] */
  def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose an [[APrism_]] with an [[IndexedSetter_]] */
  def compose[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      Forget(self.over(other.over(indexed.runIndex))).runForget
  }

  /** compose an [[APrism_]] with an [[IndexedGetter_]] */
  def compose[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose an [[APrism_]] with an [[IndexedFold_]] */
  def compose[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object APrism_ {
  /** create a polymorphic [[APrism_]] from a matcher function that produces an [[Either]] and a review function
    * <p>
    * the matcher function returns an [[Either]] to allow for type-changing prisms in the case where the input does not match.
    * </p>
    */
  def apply[S, T, A, B](_viewOrModify: S => Either[T, A])(_review: B => T): APrism_[S, T, A, B] = new APrism_[S, T, A, B] { self =>
    override private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T] = Market(_viewOrModify, _review)

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] = viewOrModify(s) match {
      case Right(a) => ev.map(f(a))(review)
      case Left(t) => ev.pure(t)
    }
  }

  /** polymorphic identity of an [[APrism_]] */
  def id[S, T]: APrism_[S, T, S, T] = APrism_[S, T, S, T](_.asRight[T])(identity)
}

object APrism {
  /** create a monomorphic [[APrism]], using preview and review functions */
  def fromPreview[S, A](preview: S => Option[A])(review: A => S): APrism[S, A] =
    APrism { s: S => preview(s).fold(s.asLeft[A])(_.asRight[S]) }(review)

  /** create a monomorphic [[APrism]], using a partial function and a review function */
  def fromPartial[S, A](preview: PartialFunction[S, A])(review: A => S): APrism[S, A] = fromPreview(preview.lift)(review)

  /** create a monomorphic [[APrism]] from a matcher function that produces an [[Either]] and a review function
    * <p>
    * the matcher function returns an [[Either]] to allow for type-changing prisms in the case where the input does not match.
    * </p>
    */
  def apply[S, A](viewOrModify: S => Either[S, A])(review: A => S): APrism[S, A] = APrism_(viewOrModify)(review)

  /** monomorphic identity of an [[APrism]] */
  def id[S]: APrism[S, S] = APrism_.id[S, S]
}
