package proptics

import cats.data.Const
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid}
import proptics.instances.BooleanInstances._
import proptics.internal._
import proptics.newtype.{Disj, First, Newtype}

import scala.Function.const

/**
  * * A [[Prism_]] with fixed type [[Market]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of an [[APrism_]]
  * @tparam T the modified source of an [[APrism_]]
  * @tparam A the focus of an [[APrism_]]
  * @tparam B the modified focus of an [[APrism_]]
  */
abstract class APrism_[S, T, A, B] { self =>
  private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T]

  /** view an optional focus of a [[APrism_]] */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  /** view the modified source of a [[APrism_]] */
  def review(b: B): T = self(Market(identity, _.asRight[B])).to(b)

  /** set the modified focus of a [[APrism_]] */
  def set(b: B): S => T = over(const(b))

  /** set the focus of a [[APrism_]] conditionally if it is not None */
  def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus type of a [[APrism_]] using a function, resulting in a change of type to the full structure  */
  def over(f: A => B): S => T = overF[Id](f)

  /** modify the focus of a [[APrism_]] using a function conditionally if it is not None, resulting in a change of type to the full structure  */
  def overOption(f: A => B): S => Option[T] = s => preview(s).map(review _ compose f)

  /** synonym for [[traverse]], flipped  */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of a [[APrism_]] using a [[cats.Functor]], resulting in a change of type to the full structure  */
  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T]

  /** tests whether there is no focus or a predicate holds for the focus of a [[APrism_]] */
  def forall(p: A => Boolean): S => Boolean = preview(_).forall(p)

  /** tests whether a predicate holds for the focus of a [[APrism_]] */
  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](f)

  /** tests whether a predicate does not hold for the focus of a [[APrism_]] */
  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** tests whether the focus of a [[APrism_]] contains a given value */
  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** tests whether the focus of a [[APrism_]] does not contain a given value */
  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  /** check if the [[APrism_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[APrism_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** finds if the focus of a [[APrism_]] is satisfying a predicate. */
  def find(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  /** convert an [[APrism_]] to the pair of functions that characterize it */
  def withPrism[R](f: (S => Either[T, A]) => (B => T) => R): R = {
    val market = self(Market(identity, _.asRight[B]))

    f(market.from)(market.to)
  }

  /** retrieve the focus of an [[APrism_]] or return the original value while allowing the type to change if it does not match */
  def matching(s: S): Either[T, A] = withPrism(either => const(either.apply(s)))

  /** transform an [[APrism_]] to a [[Prism_]] */
  def asPrism: Prism_[S, T, A, B] = withPrism(Prism_[S, T, A, B])

  /** compose [[APrism_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] =
      self(Market(identity, _.asRight[B])) compose other(market)

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  /** compose [[APrism_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): APrism_[S, T, C, D] = self compose other.asIso

  /** compose [[APrism_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pcd: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pcd)
    }
  }

  /** compose [[APrism_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pcd: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pcd)
    }
  }

  /** compose [[APrism_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] =
      self(Market(identity, _.asRight[B])) compose other(market)

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  /** compose [[APrism_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): APrism_[S, T, C, D] = self compose other.asPrism

  /** compose [[APrism_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pcd: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pcd)
    }
  }

  /** compose [[APrism_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(f))
    })

  /** compose [[APrism_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = s => {
      val market = self(Market(identity[B], _.asRight[B]))

      market.from(s).fold(identity, self.review _ compose other(pab))
    }
  }

  /** compose [[APrism_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose [[APrism_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose [[APrism_]] with a [[Review_]] */
  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = self.asPrism compose other

  private def foldMapNewtype[F: Monoid, R](f: A => R)(s: S)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object APrism_ {
  /** create an polymorphic [[APrism_]], using an operation which returns an [[Option]] */
  def fromOption[S, A](from: S => Option[A])(to: A => S): APrism[S, A] =
    APrism { s: S => from(s).fold(s.asLeft[A])(_.asRight[S]) }(to)

  /**
   * create a polymorphic [[APrism_]] from a matcher function that produces an [[Either]] and a constructor
   * <p>
   * the matcher function returns an [[Either]] to allow for type-changing prisms in the case where the input does not match.
   * </p>
   */
  def apply[S, T, A, B](from: S => Either[T, A])(to: B => T): APrism_[S, T, A, B] = new APrism_[S, T, A, B] { self =>
    override private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T] = Market(to, from)

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] = from(s) match {
      case Right(a) => ev.map(f(a))(to)
      case Left(t)  => ev.pure(t)
    }
  }
}

object APrism {
  /** create a monomorphic [[APrism]], using an operation which returns an [[Option]] */
  def fromOption[S, A](from: S => Option[A])(to: A => S): APrism[S, A] =
    APrism { s: S => from(s).fold(s.asLeft[A])(_.asRight[S]) }(to)

  /**
   * create a monomorphic [[APrism]] from a matcher function that produces an [[Either]] and a constructor
   * <p>
   * the matcher function returns an [[Either]] to allow for type-changing prisms in the case where the input does not match.
   * </p>
   */
  def apply[S, A](from: S => Either[S, A])(to: A => S): APrism[S, A] = APrism_(from)(to)
}
