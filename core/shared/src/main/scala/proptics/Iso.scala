package proptics

import cats.arrow.{Profunctor, Strong}
import cats.instances.function._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.either._
import cats.{Applicative, Comonad, Eq, Monoid}
import proptics.internal._
import proptics.profunctor.{Choice, Closed, Costar}
import proptics.rank2types._
import proptics.syntax.function._

import scala.Function.const
import scala.{Function => F}

/**
  * A generalized isomorphism.
  *  [[Iso_]] is complete reversible transformation between two types
  *
  * @tparam S the source of an [[Iso_]]
  * @tparam T the modified source of an [[Iso_]]
  * @tparam A the target of an [[Iso_]]
  * @tparam B the modified target of a [[Iso_]]
  */
abstract class Iso_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T]

  /** view the target of an [[Iso_]] */
  def view(s: S): A = self[Forget[A, *, *]](Forget(identity[A])).runForget(s)

  /** view the modified source of an [[Iso_]] */
  def review(b: B): T = self(Tagged[A, B](b))(Tagged.choiceTagged).runTag

  /** set the modified target of an [[Iso_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the target type of an [[Iso_]] using a function, resulting in a change of type to the full structure  */
  def over(f: A => B): S => T = self(f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the target type of an [[Iso_]] using a [[cats.Functor]], resulting in a change of type to the full structure  */
  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] = ev.map(f(self.view(s)))(self.set(_)(s))

  /** finds if the target of an [[Iso_]] is satisfying a predicate. */
  def find(f: A => Boolean): S => Option[A] = s => view(s).some.filter(f)

  /** tests whether a predicate holds for the target of an [[Iso_]] */
  def exists(f: A => Boolean): S => Boolean = f compose view

  /** tests whether a predicate does not hold for the target of an [[Iso_]] */
  def noExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** tests whether the target contains a given value */
  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** tests whether the target does not contain a given value */
  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  /** zip two sources of an [[Iso_]] together provided a binary operation which modify the target type of an [[Iso_]] */
  def zipWith[F[_]](f: A => A => B): S => S => T = self(Zipping(f))(Zipping.closedZipping).runZipping

  /** modify an effectual target of an [[Iso_]] into the modified target, resulting in a change of type to the full structure  */
  def cotraverse[F[_]: Comonad](fs: F[S])(f: F[A] => B)(implicit ev: Applicative[F]): T =
    self(Costar(f))(Costar.profunctorCostar[F](ev)).runCostar(fs)

  /** synonym for [[cotraverse]], flipped */
  def zipWithF[F[_]: Comonad: Applicative](f: F[A] => B)(fs: F[S]): T = cotraverse(fs)(f)

  /** reverses an [[Iso_]] by swapping the source and the target */
  def reverse: Iso_[B, A, T, S] = new Iso_[B, A, T, S] {
    override def apply[P[_, _]](pab: P[T, S])(implicit ev: Profunctor[P]): P[B, A] =
      self(Re(identity[P[B, A]])).runRe(pab)
  }

  /** compose [[Iso_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): Iso_[S, T, C, D] = new Iso_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Profunctor[P]): P[S, T] = self(other(pab))
  }

  /** compose [[Iso_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): AnIso_[S, T, C, D] = new AnIso_[S, T, C, D] {
    override private[proptics] def apply(exchange: Exchange[C, D, C, D]): Exchange[C, D, S, T] = self(other(exchange))

    override def review(d: D): T = self.review(other.review(d))
  }

  /** compose [[Iso_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): Lens_[S, T, C, D] = new Lens_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Strong[P]): P[S, T] = self(other(pab))
  }

  /** compose [[Iso_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] = self(other(shop))
  }

  /** compose [[Iso_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): Prism_[S, T, C, D] = new Prism_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Choice[P]): P[S, T] = self(other(pab))
  }

  /** compose [[Iso_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): APrism_[S, T, C, D] = new APrism_[S, T, C, D] {
    override private[proptics] def apply(market: Market[C, D, C, D]): Market[C, D, S, T] = self(other(market))

    override def traverse[F[_]](s: S)(f: C => F[D])(implicit ev: Applicative[F]): F[T] = {
      val market = self(other(Market[C, D, C, D](identity, _.asRight[D])))

      market.from(s).fold(ev.pure, c => ev.map(f(c))(market.to))
    }
  }

  /** compose [[Iso_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose [[Iso_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose [[Iso_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose [[Iso_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]) = self(other(Forget(identity)))
  }

  /** compose [[Iso_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))(Forget.wanderForget)
  }

  /** compose [[Iso_]] with a [[Grate_]] */
  def compose[C, D](other: Grate_[A, B, C, D]): Grate_[S, T, C, D] = new Grate_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Closed[P]): P[S, T] = self(other(pab))
  }

  /** compose [[Iso_]] with a [[Review_]] */
  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))(Tagged.choiceTagged)
  }
}

object Iso_ {
  /** create a polymorphic [[Iso_]] from Rank2TypeIsoLike encoding */
  private[proptics] def apply[S, T, A, B](f: Rank2TypeIsoLike[S, T, A, B]): Iso_[S, T, A, B] = new Iso_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T] = f(pab)
  }

  /** create an [[Iso_]] from pair of functions
    * {{{
    * from -> from the source of an [[Iso_]] to the target of an [[Iso_]],
    * to -> from the modified target of an [[Iso_]] to the modified source of an [[Iso_]]
    * }}}
    */
  def apply[S, T, A, B](from: S => A)(to: B => T): Iso_[S, T, A, B] = iso(from)(to)

  /** synonym to [[apply]] */
  def iso[S, T, A, B](from: S => A)(to: B => T): Iso_[S, T, A, B] =
    Iso_(new Rank2TypeIsoLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T] = ev.dimap(pab)(from)(to)
    })

  /** an isomorphism for currying and uncurrying a function */
  def curried[A, B, C, D, E, F]: Iso_[(A, B) => C, (D, E) => F, A => B => C, D => E => F] =
    iso[(A, B) => C, (D, E) => F, A => B => C, D => E => F](_.curried)(F.uncurried[D, E, F])

  /** an isomorphism for uncurrying and currying a function */
  def uncurried[A, B, C, D, E, F]: Iso_[A => B => C, D => E => F, (A, B) => C, (D, E) => F] =
    iso[A => B => C, D => E => F, (A, B) => C, (D, E) => F](F.uncurried[A, B, C])(_.curried)

  /** an isomorphism for flipping a function */
  def flipped[A, B, C, D, E, F]: Iso_[A => B => C, D => E => F, B => A => C, E => D => F] =
    iso[A => B => C, D => E => F, B => A => C, E => D => F](_.flip)(_.flip)
}

object Iso {
  /** create a monomorphic [[Iso_]] from Rank2TypeIsoLike encoding */
  private[proptics] def apply[S, A](f: Rank2TypeIsoLike[S, S, A, A]): Iso[S, A] = new Iso[S, A] {
    override def apply[P[_, _]](pab: P[A, A])(implicit ev: Profunctor[P]): P[S, S] = f(pab)
  }

  /** create a monomorphic [[Iso_]] from pair of functions
    * {{{
    * from -> from the source of an [[Iso_]] to the target of an [[Iso_]],
    * to -> from the target of an [[Iso_]] to the source of an [[Iso_]]
    * }}}
    */
  def apply[S, A](from: S => A)(to: A => S): Iso[S, A] = Iso_.iso(from)(to)

  /** synonym to [[apply]] */
  def iso[S, A](from: S => A)(to: A => S): Iso[S, A] = Iso_.iso(from)(to)

  /** if `A1` is obtained from `A` by removing a single value, then `Option[A1]` is isomorphic to `A` */
  def non[A](a: A)(implicit ev: Eq[A]): Iso[Option[A], A] = {
    def g(a1: A): Option[A] = if (a1 === a) None else a.some

    Iso_.iso((op: Option[A]) => op.getOrElse(a))(g)
  }
}
