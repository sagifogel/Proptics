package proptics

import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Applicative, Comonad, Eq, Monoid}
import proptics.internal.{Forget, Indexed, Shop, Traversing, Wander, Zipping}
import proptics.newtype.Disj
import proptics.syntax.tuple._
import proptics.profunctor.{Costar, Star}
import proptics.rank2types.Rank2TypeIndexedLensLike

import scala.Function.const

/** [[IndexedLens_]] is An IndexedOptic constrained with [[Strong]] [[cats.arrow.Profunctor]]
  *
  * @tparam I the index of an [[IndexedLens_]]
  * @tparam S the source of an [[IndexedLens_]]
  * @tparam T the modified source of an [[IndexedLens_]]
  * @tparam A the target of an [[IndexedLens_]]
  * @tparam B the modified target of an [[IndexedLens_]]
  */
abstract class IndexedLens_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T]

  def view(s: S): (I, A) = self[Forget[(I, A), *, *]](Indexed(Forget(identity))).runForget(s)

  def set(b: B): S => T = over(const(b))

  def over(f: ((I, A)) => B): S => T = self(Indexed(f))

  def overF[F[_]: Applicative](f: ((I, A)) => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]: Applicative](s: S)(f: ((I, A)) => F[B]): F[T] = self(Indexed(Star(f))).runStar(s)

  def filter(f: ((I, A)) => Boolean): S => Option[(I, A)] = s => view(s).some.filter(f)

  def exists(f: ((I, A)) => Boolean): S => Boolean = f compose view

  def noExists(f: ((I, A)) => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = !contains(s)(a)

  def failover[F[_]](f: ((I, A)) => B)(s: S)(implicit ev0: Strong[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] = {
    val star = Star[(Disj[Boolean], *), (I, A), B](ia => (Disj(true), f(ia)))

    self(Indexed(star)).runStar(s) match {
      case (Disj(true), x)  => ev1.pure(x)
      case (Disj(false), _) => ev1.empty
    }
  }

  def zipWith[F[_]](f: ((I, A)) => ((I, A)) => B): S => S => T = self(Indexed(Zipping(f))).runZipping

  def zipWithF[F[_]: Comonad](fs: F[S])(f: F[(I, A)] => B): T = self(Indexed(Costar(f))).runCostar(fs)

  def asLens: Lens_[S, T, A, B] = new Lens_[S, T, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] =
      self(Indexed(ev.lmap[A, B, (I, A)](pab)(_._2)))
  }

  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedLens_[I, S, T, C, D] = new IndexedLens_[I, S, T, C, D] {
    override private[proptics] def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Strong[P]): P[S, T] =
      self(Indexed(ev.lmap[A, B, (I, A)](other(indexed))(_._2)))
  }

  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    override def apply(indexed: Indexed[Shop[(I, C), D, *, *], I, C, D]): Shop[(I, C), D, S, T] =
      Shop(other.view _ compose Tuple2._2[I, A] compose self.view, s => d => self.set(other.set(d)(self.view(s)._2))(s))
  }

  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (I, C), D] = new Traversing[S, T, (I, C), D] {
        override def apply[F[_]](f: ((I, C)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (_, a) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  def compose[C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B](other(indexed) compose Tuple2._2))
  }

  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(I, C), *, *], I, C, D]): Forget[(I, C), S, T] =
      Forget(other.view _ compose Tuple2._2[I, A] compose self.view)
  }

  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._2)(indexed.runIndex.runForget))
  }
}

object IndexedLens_ {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): IndexedLens_[I, S, T, A, B] = new IndexedLens_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T] = f(indexed.runIndex)
  }

  def apply[I, S, T, A, B](to: S => ((I, A), B => T)): IndexedLens_[I, S, T, A, B] =
    IndexedLens_(new Rank2TypeIndexedLensLike[I, S, T, A, B] {
      override def apply[P[_, _]](piab: P[(I, A), B])(implicit ev: Strong[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(piab)
    })

  def apply[I, S, T, A, B](get: S => (I, A))(set: S => B => T): IndexedLens_[I, S, T, A, B] =
    IndexedLens_((get, set).mapN(Tuple2.apply))

  private[proptics] def liftIndexedOptic[P[_, _], I, S, T, A, B](to: S => ((I, A), B => T))(implicit ev: Strong[P]): P[(I, A), B] => P[S, T] =
    piab => ev.dimap(ev.first[(I, A), B, B => T](piab))(to) { case (b, b2t) => b2t(b) }
}

object IndexedLens {
  def apply[I, S, A](to: S => ((I, A), A => S)): IndexedLens[I, S, A] = IndexedLens_(to)

  def apply[I, S, A](get: S => (I, A))(set: S => A => S): IndexedLens[I, S, A] = IndexedLens_(get)(set)
}
