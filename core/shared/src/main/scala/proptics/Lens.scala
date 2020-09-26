package proptics

import cats.arrow.Strong
import cats.data.State
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Applicative, Comonad, Eq, Functor, Monoid}
import proptics.internal._
import proptics.newtype.Disj
import proptics.profunctor.{Choice, Costar, Star, Wander}
import proptics.rank2types.Rank2TypeLensLike
import proptics.syntax.star._

import scala.Function.const

/**
  * Given a type whose "focus element" always exists,
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
  def view(s: S): A = self[Forget[A, *, *]](Forget(identity)).runForget(s)

  /** set the modified focus of a [[Lens_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the focus type of a [[Lens_]] using a function, resulting in a change of type to the full structure */
  def over(f: A => B): S => T = self(f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Functor](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of a [[Lens_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]: Functor](s: S)(f: A => F[B]): F[T] = self(Star(f)).runStar(s)

  /** test whether a predicate holds for the focus of a [[Lens_]] */
  def exists(f: A => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of a [[Lens_]] */
  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of a [[Lens_]] contains a given value */
  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus a [[Lens_]] does not contain a given value */
  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** find if the focus of a [[Lens_]] is satisfying a predicate. */
  def find(f: A => Boolean): S => Option[A] = s => view(s).some.filter(f)

  /** view the focus of a [[Lens_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, A] = ev.inspect(view)

  /** try to map a function over this [[Lens_]], failing if the [[Lens_]] has no focus. */
  def failover[F[_]](f: A => B)(s: S)(implicit ev0: Strong[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] = {
    val star = Star[(Disj[Boolean], *), A, B](a => (Disj(true), f(a)))

    self(star).runStar(s) match {
      case (Disj(true), x)  => ev1.pure(x)
      case (Disj(false), _) => ev1.empty
    }
  }

  /** zip two sources of a [[Lens_]] together provided a binary operation which modify the focus type of a [[Lens_]] */
  def zipWith[F[_]](s1: S, s2: S)(f: (A, A) => B): T = self(Zipping(f.curried)).runZipping(s1)(s2)

  /** modify an effectual focus of an [[Lens_]] into the modified focus, resulting in a change of type to the full structure */
  def cotraverse[F[_]: Comonad](fs: F[S])(f: F[A] => B)(implicit ev: Applicative[F]): T = self(Costar(f)).runCostar(fs)

  /** synonym for [[cotraverse]], flipped */
  def zipWithF[F[_]: Comonad](f: F[A] => B)(fs: F[S]): T = self(Costar(f)).runCostar(fs)

  /** compose a [[Lens_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): Lens_[S, T, C, D] = new Lens_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Strong[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Lens_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): Lens_[S, T, C, D] = self compose other.asIso

  /** compose a [[Lens_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): Lens_[S, T, C, D] = new Lens_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Strong[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Lens_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] = self(other(shop))
  }

  /** compose a [[Lens_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Lens_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asPrism

  /** compose a [[Lens_]] with a [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))

    /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, C] = other.viewOrModify(self.view(s)).leftMap(self.set(_)(s))
  }

  /** compose [[Lens_]] with an [[AnAffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      other.viewOrModify(self.view(s)).leftMap(self.set(_)(s))
    }(s => d => self.over(other.set(d))(s))

  /** compose a [[Lens_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose a [[Lens_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] = ATraversal_(new RunBazaar[* => *, C, D, S, T] {
    override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
      self.traverse(s)(other.traverse(_)(pafb))
  })

  /** compose a [[Lens_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Lens_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] = self(other(Forget(identity)))
  }

  /** compose a [[Lens_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }
}

object Lens_ {

  /** create a polymorphic [[Lens_]] from Rank2TypeLensLike encoding */
  private[proptics] def apply[S, T, A, B](f: Rank2TypeLensLike[S, T, A, B]): Lens_[S, T, A, B] = new Lens_[S, T, A, B] { self =>
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] = f(pab)
  }

  /** create a polymorphic [[Lens_]] from a getter/setter pair */
  def apply[S, T, A, B](view: S => A)(set: S => B => T): Lens_[S, T, A, B] = Lens_.lens((view, set).mapN(Tuple2.apply))

  /** create a polymorphic [[Lens_]] from a combined getter/setter */
  def lens[S, T, A, B](to: S => (A, B => T)): Lens_[S, T, A, B] = Lens_(new Rank2TypeLensLike[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] = liftOptic(to)(ev)(pab)
  })

  /** polymorphic identity of a [[Lens_]] */
  def id[S, T]: Lens_[S, T, S, T] = Lens_[S, T, S, T](identity[S] _)(const(identity))

  /** lift a combined getter/setter function to a general optic using [[Strong]] profunctor */
  private[proptics] def liftOptic[P[_, _], S, T, A, B](to: S => (A, B => T))(implicit ev: Strong[P]): P[A, B] => P[S, T] =
    pab => ev.dimap(ev.first[A, B, B => T](pab))(to) { case (b, f) => f(b) }
}

object Lens {

  /** create a momnomorphic [[Lens]] from a getter/setter pair */
  def apply[S, A](view: S => A)(set: S => A => S): Lens[S, A] = Lens_[S, S, A, A](view)(set)

  /** create a momnomorphic [[Lens]] from a combined getter/setter function */
  def lens[S, A](to: S => (A, A => S)): Lens[S, A] = Lens_.lens[S, S, A, A](to)

  /** momnomorphic identity of a [[Lens]] */
  def id[S]: Lens[S, S] = Lens_.id[S, S]
}
