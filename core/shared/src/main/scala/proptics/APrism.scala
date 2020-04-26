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
  * @tparam A the target of an [[APrism_]]
  * @tparam B the modified target of an [[APrism_]]
  */
abstract class APrism_[S, T, A, B] { self =>
  private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T]

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def review(b: B): T = self(Market(identity, _.asRight[B])).to(b)

  def set(b: B): S => T = over(const(b))

  def setOption(b: B): S => Option[T] = overOption(const(b))

  def over(f: A => B): S => T = overF[Id](f)

  def overOption(f: A => B): S => Option[T] = s => preview(s).map(review _ compose f)

  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T]

  def isEmpty(s: S): Boolean = preview(s).isEmpty

  def nonEmpty(s: S): Boolean = !isEmpty(s)

  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](f)

  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def filter(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  def forall(p: A => Boolean): S => Boolean = preview(_).forall(p)

  def withPrism[R](f: (B => T) => (S => Either[T, A]) => R): R = {
    val market = self(Market(identity, _.asRight[B]))

    f(market.to)(market.from)
  }

  def matching(s: S): Either[T, A] = withPrism(const(_.apply(s)))

  def asPrism: Prism_[S, T, A, B] = withPrism(Prism_[S, T, A, B])

  def compose[C, D](other: Iso_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] =
      self(Market(identity, _.asRight[B])) compose other(market)

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): APrism_[S, T, C, D] = self compose other.asIso

  def compose[C, D](other: Lens_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pcd: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pcd)
    }
  }

  def compose[C, D](other: ALens_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pcd: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pcd)
    }
  }

  def compose[C, D](other: Prism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] =
      self(Market(identity, _.asRight[B])) compose other(market)

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  def compose[C, D](other: APrism_[A, B, C, D]): APrism_[S, T, C, D] = self compose other.asPrism

  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pcd: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pcd)
    }
  }

  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(f))
    })

  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = s => {
      val market = self(Market(identity[B], _.asRight[B]))

      market.from(s).fold(identity, self.review _ compose other(pab))
    }
  }

  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = self.asPrism compose other

  private def foldMapNewtype[F: Monoid, R](f: A => R)(s: S)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object APrism_ {
  def apply[S, T, A, B](to: B => T)(from: S => Either[T, A]): APrism_[S, T, A, B] = new APrism_[S, T, A, B] { self =>
    override private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T] = Market(to, from)

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] = from(s) match {
      case Right(a) => ev.map(f(a))(to)
      case Left(t)  => ev.pure(t)
    }
  }
}

object APrism {
  def apply[S, A](to: A => S)(from: S => Either[S, A]): APrism[S, A] = APrism_(to)(from)
}
