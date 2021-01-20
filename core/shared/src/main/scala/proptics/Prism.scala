package proptics

import scala.Function.const

import cats.arrow.Strong
import cats.data.Const
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Applicative, Eq, Monoid}
import spire.algebra.lattice.Heyting
import spire.std.boolean._

import proptics.IndexedTraversal_.wander
import proptics.internal._
import proptics.newtype.{Conj, Disj, First, Newtype}
import proptics.profunctor.{Choice, Star, Wander}
import proptics.rank2types.{LensLikeWithIndex, Rank2TypePrismLike}
import proptics.syntax.star._

/** @tparam S the source of a [[Prism_]]
  * @tparam T the modified source of a [[Prism_]]
  * @tparam A the focus of a [[Prism_]]
  * @tparam B the modified focus of a [[Prism_]]
  */
abstract class Prism_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]

  /** view the focus of a [[Prism_]] or return the modified source of a [[Prism_]] */
  def viewOrModify(s: S): Either[T, A]

  /** view an optional focus of a [[Prism_]] */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  /** view the modified source of a [[Prism_]] */
  def review(b: B): T = self(Tagged[A, B](b)).runTag

  /** set the modified focus of a [[Prism_]] */
  def set(b: B): S => T = over(const(b))

  /** set the focus of a [[Prism_]] conditionally if it is not None */
  def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus type of a [[Prism_]] using a function, resulting in a change of type to the full structure */
  def over(f: A => B): S => T = self(f)

  /** modify the focus of a [[Prism_]] using a function conditionally if it is not None, resulting in a change of type to the full structure */
  def overOption(f: A => B): S => Option[T] = s => preview(s).map(review _ compose f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of a [[Prism_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  /** test whether there is no focus or a predicate holds for the focus of a [[Prism_]] */
  def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for the focus of a [[Prism_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** test whether a predicate holds for the focus of a [[Prism_]] */
  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](_)(f)

  /** test whether a predicate does not hold for the focus of a [[Prism_]] */
  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of a [[Prism_]] contains a given value */
  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus of a [[Prism_]] does not contain a given value */
  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** check if the [[Prism_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[Prism_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** find if the focus of a [[Prism_]] is satisfying a predicate. */
  def find(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** transform a [[Prism_]] to an [[APrism_]] */
  def asAPrism: APrism_[S, T, A, B] = APrism_(viewOrModify)(review)

  /** transform a [[Prism_]] to a [[Fold_]] */
  def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.preview(_).fold(Monoid[R].empty)(forget.runForget))
  }

  /** compose a [[Prism_]] with a function lifted to a [[Getter_]] */
  def to[C, D](f: A => C): Fold_[S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose a [[Prism_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): Prism_[S, T, C, D] = new Prism_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Choice[P]): P[S, T] = self(other(pab))

    /** view the focus of a [[Prism_]] or return the modified source of a [[Prism_]] */
    override def viewOrModify(s: S): Either[T, C] = self.viewOrModify(s).map(other.view)
  }

  /** compose a [[Prism_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): Prism_[S, T, C, D] = self compose other.asIso

  /** compose a [[Prism_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, C] = self.viewOrModify(s).map(other.view)
  }

  /** compose a [[Prism_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self compose other.asLens

  /** compose a [[Prism_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): Prism_[S, T, C, D] = new Prism_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Choice[P]): P[S, T] = self(other(pab))

    /** view the focus of a [[Prism_]] or return the modified source of a [[Prism_]] */
    override def viewOrModify(s: S): Either[T, C] =
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
  }

  /** compose a [[Prism_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] = self(other(market))

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  /** compose a [[Prism_]] with an [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, C] =
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
  }

  /** compose a [[Prism_]] with an [[AffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))) }(s => d => self.over(other.set(d))(s))

  /** compose a [[Prism_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Prism_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] = ATraversal_(new RunBazaar[* => *, C, D, S, T] {
    override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] = {
      val bazaar = other(new Bazaar[* => *, C, D, C, D] {
        override def runBazaar: RunBazaar[* => *, C, D, C, D] = new RunBazaar[* => *, C, D, C, D] {
          override def apply[G[_]](pafb: C => G[D])(s: C)(implicit ev: Applicative[G]): G[D] = pafb(s)
        }
      })

      self(bazaar)(Bazaar.wanderBazaar).runBazaar(pafb)(s)
    }
  })

  /** compose a [[Prism_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Prism_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold

  /** compose a [[Prism_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose a [[Prism_]] with a [[Review_]] */
  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))(Tagged.choiceTagged)
  }

  /** compose a [[Prism_]] with an [[IndexedLens_]] */
  def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose a [[Prism_]] with an [[AnIndexedLens_]] */
  def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose a [[Prism_]] with an [[IndexedTraversal_]] */
  def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose an [[Prism_]] with an [[IndexedFold_]] */
  def compose[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }
}

object Prism_ {
  /** create a polymorphic [[Prism_]] from Rank2TypePrismLike encoding */
  private[proptics] def apply[S, T, A, B](prismLike: Rank2TypePrismLike[S, T, A, B] with PrismFunctions[S, T, A]): Prism_[S, T, A, B] = new Prism_[S, T, A, B] { self =>
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = prismLike(pab)

    /** view the focus of a [[Prism_]] or return the modified source of a [[Prism_]] */
    override def viewOrModify(s: S): Either[T, A] = prismLike.viewOrModify(s)
  }

  /** create a polymorphic [[Prism_]] from a matcher function that produces an [[Either]] and a review function
    * <p>
    * the matcher function returns an [[Either]] to allow for type-changing prisms in the case where the input does not match.
    * </p>
    */
  def apply[S, T, A, B](_viewOrModify: S => Either[T, A])(review: B => T): Prism_[S, T, A, B] =
    Prism_(new Rank2TypePrismLike[S, T, A, B] with PrismFunctions[S, T, A] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = {
        val right: P[Either[T, A], Either[T, T]] = ev.right[A, T, T](ev.rmap(pab)(review))

        ev.dimap(right)(viewOrModify)(_.fold(identity, identity))
      }

      override def viewOrModify(s: S): Either[T, A] = _viewOrModify(s)
    })

  /** polymorphic identity of a [[Prism_]] */
  def id[S, T]: Prism_[S, T, S, T] = Prism_[S, T, S, T] { s: S => s.asRight[T] }(identity[T])

  /** implicit conversion from [[APrism_]] to [[Prism_]] */
  implicit def aPrismToPrism[S, T, A, B](aPrism: APrism_[S, T, A, B]): Prism_[S, T, A, B] = aPrism.asPrism
}

object Prism {
  /** create a monomorphic [[Prism]], using preview and review functions */
  def fromPreview[S, A](preview: S => Option[A])(review: A => S): Prism[S, A] =
    Prism { s: S => preview(s).fold(s.asLeft[A])(_.asRight[S]) }(review)

  /** create a monomorphic [[Prism]], using a partial function and review functions */
  def fromPartial[S, A](preview: PartialFunction[S, A])(review: A => S): Prism[S, A] = fromPreview(preview.lift)(review)

  /**  create a polymorphic [[Prism]] from a matcher function that produces an [[Either]] and a review function
    *  <p>
    *  the matcher function returns an [[Either]] to allow for type-changing prisms in the case where the input does not match.
    *  </p>
    */
  def apply[S, A](viewOrModify: S => Either[S, A])(review: A => S): Prism[S, A] = Prism_(viewOrModify)(review)

  /** create a monomorphic [[Prism]] that checks whether the focus matches a predicate */
  def nearly[A](a: A)(predicate: A => Boolean)(implicit ev: Alternative[Option]): Prism[A, Unit] =
    Prism.fromPreview[A, Unit](ev.guard _ compose predicate)(const(a))

  /** create a monomorphic [[Prism]] that checks whether the focus matches a single value */
  def only[A: Eq](a: A)(implicit ev: Alternative[Option]): Prism[A, Unit] = nearly(a)(_ === a)

  /** monomorphic identity of a [[Prism]] */
  def id[S]: Prism[S, S] = Prism_.id[S, S]
}
