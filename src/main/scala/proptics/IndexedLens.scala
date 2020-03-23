package proptics

import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Applicative, Comonad, Eq}
import proptics.internal.{Forget, Indexed, Zipping}
import proptics.newtype.Disj
import proptics.profunctor.{Costar, Star}
import proptics.rank2types.Rank2TypeIndexedLensLike

import scala.Function.const

/** [[IndexedLens]] is An IndexedOptic constrained with [[Strong]] [[cats.arrow.Profunctor]]
 *
 * @tparam I the index of an [[IndexedLens]]
 * @tparam S the source of an [[IndexedLens]]
 * @tparam T the modified source of an [[IndexedLens]]
 * @tparam A the target of an [[IndexedLens]]
 * @tparam B the modified target of an [[IndexedLens]]
 */
abstract class IndexedLens[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T]

  def view(s: S): (I, A) = self[Forget[(I, A), *, *]](Indexed(Forget(identity))).runForget(s)

  def set(b: B): S => T = over(const(b))

  def over(f: ((I, A)) => B): S => T = self(Indexed(f))

  def overF[F[_]: Applicative](f: ((I, A)) => F[B])(s: S): F[T] = traverse(f)(s)

  def traverse[F[_]: Applicative](f: ((I, A)) => F[B])(s: S): F[T] = self(Indexed(Star(f))).runStar(s)

  def filter(f: ((I, A)) => Boolean): S => Option[(I, A)] = s => view(s).some.filter(f)

  def exists(f: ((I, A)) => Boolean): S => Boolean = f compose view

  def noExists(f: ((I, A)) => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = !contains(s)(a)

  def failover[F[_]](f: ((I, A)) => B)(s: S)(implicit ev0: Strong[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] = {
    val star = Star[(Disj[Boolean], *), (I, A), B](ia => (Disj(true), f(ia)))

    self(Indexed(star)).runStar(s) match {
      case (Disj(true), x) => ev1.pure(x)
      case (Disj(false), _) => ev1.empty
    }
  }

  def zipWith[F[_]](f: ((I, A)) => ((I, A)) => B): S => S => T = self(Indexed(Zipping(f))).runZipping

  def zipWithF[F[_]: Comonad](fs: F[S])(f: F[(I, A)] => B): T = self(Indexed(Costar(f))).runCostar(fs)

  def asLens: Lens[S, T, A, B] = new Lens[S, T, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] =
      self(Indexed(ev.dimap[A, B, (I, A), B](pab)(_._2)(identity)))
  }
}

object IndexedLens {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): IndexedLens[I, S, T, A, B] = new IndexedLens[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T] = f(indexed.runIndex)
  }

  def apply[I, S, T, A, B](to: S => ((I, A), B => T)): IndexedLens[I, S, T, A, B] =
    IndexedLens(new Rank2TypeIndexedLensLike[I, S, T, A, B] {
      override def apply[P[_, _]](piab: P[(I, A), B])(implicit ev: Strong[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(piab)
    })

  def apply[I, S, T, A, B](get: S => (I, A))(set: S => B => T): IndexedLens[I, S, T, A, B] =
    IndexedLens((get, set).mapN(Tuple2.apply))

  private[proptics] def liftIndexedOptic[P[_, _], I, S, T, A, B](to: S => ((I, A), B => T))(implicit ev: Strong[P]): P[(I, A), B] => P[S, T] =
    piab => ev.dimap(ev.first[(I, A), B, B => T](piab))(to) { case (b, b2t) => b2t(b) }
}

object IndexedLens_ {
  def apply[I, S, A](to: S => ((I, A), A => S)): IndexedLens_[I, S, A] = IndexedLens(to)

  def apply[I, S, A](get: S => (I, A))(set: S => A => S): IndexedLens_[I, S, A] = IndexedLens(get)(set)
}
