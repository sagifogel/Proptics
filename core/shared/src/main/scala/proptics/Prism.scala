package proptics

import cats.data.Const
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Applicative, Comonad, Eq, Monoid}
import proptics.instances.BooleanInstances._
import proptics.internal._
import proptics.newtype.{Disj, First, Newtype}
import proptics.profunctor.{Choice, Costar, Star}
import proptics.rank2types.Rank2TypePrismLike

import scala.Function.const

/**
  * @tparam S the source of a [[Prism_]]
  * @tparam T the modified source of a [[Prism_]]
  * @tparam A the target of a [[Prism_]]
  * @tparam B the modified target of a [[Prism_]]
  */
abstract class Prism_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def review(b: B): T = self(Tagged[A, B](b)).runTag

  def set(b: B): S => T = over(const(b))

  def setOption(b: B): S => Option[T] = overOption(const(b))

  def over(f: A => B): S => T = self(f)

  def overOption(f: A => B): S => Option[T] = s => preview(s).map(review _ compose f)

  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  def isEmpty(s: S): Boolean = preview(s).isEmpty

  def nonEmpty(s: S): Boolean = !isEmpty(s)

  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](f)

  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def filter(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  def forall(p: A => Boolean): S => Boolean = preview(_).forall(p)

  def zipWith[F[_]](f: A => A => B): S => S => T = self(Zipping(f)).runZipping

  def zipWithF[F[_]: Comonad: Applicative](fs: F[S])(f: F[A] => B): T = self(Costar(f)).runCostar(fs)

  private def foldMapNewtype[F: Monoid, R](f: A => R)(s: S)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  def compose[C, D](other: Iso_[A, B, C, D]): Prism_[S, T, C, D] = new Prism_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Choice[P]) = self(other(pab))
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): Prism_[S, T, C, D] = self compose other.asIso_

  def compose[C, D](other: Lens_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]) = self(other(pab))
  }

  def compose[C, D](other: ALens_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asLens_

  def compose[C, D](other: Prism_[A, B, C, D]): Prism_[S, T, C, D] = new Prism_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Choice[P]) = self(other(pab))
  }

  def compose[C, D](other: APrism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]) = self(other(market))

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(f))
  }

  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]) = self(other(pab))
  }

  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] = {
        val bazaar = other(new Bazaar[* => *, C, D, C, D] {
          override def runBazaar: RunBazaar[* => *, C, D, C, D] = new RunBazaar[* => *, C, D, C, D] {
            override def apply[G[_]](pafb: C => G[D])(s: C)(implicit ev: Applicative[G]): G[D] = pafb(s)
          }
        })

        self(bazaar).runBazaar(pafb)(s)
      }
    })

  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D) = self(other(pab))
  }

  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold_

  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]) = self(other(forget))
  }

  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))(Tagged.choiceTagged)
  }
}

object Prism_ {
  private[proptics] def apply[S, T, A, B](prismLike: Rank2TypePrismLike[S, T, A, B]): Prism_[S, T, A, B] = new Prism_[S, T, A, B] { self =>
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = prismLike(pab)
  }

  def apply[S, T, A, B](to: B => T)(from: S => Either[T, A]): Prism_[S, T, A, B] =
    Prism_(new Rank2TypePrismLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = {
        val right = ev.right[T, A, T](ev.rmap(pab)(to))

        ev.dimap(right)(from)(_.fold(identity, identity))
      }
    })
}

object Prism {
  def fromOption[S, A](to: A => S)(from: S => Option[A]): Prism[S, A] =
    Prism(to)(s => from(s).fold(s.asLeft[A])(_.asRight[S]))

  def apply[S, A](to: A => S)(from: S => Either[S, A]): Prism[S, A] = Prism_(to)(from)

  def nearly[A](a: A)(predicate: A => Boolean)(implicit ev: Alternative[Option]): Prism[A, Unit] =
    Prism.fromOption[A, Unit](const(a))(ev.guard _ compose predicate)

  def only[A: Eq](a: A)(implicit ev: Alternative[Option]): Prism[A, Unit] = nearly(a)(_ === a)
}
