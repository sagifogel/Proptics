package proptics

import scala.Function.const

import cats.arrow.Strong
import cats.data.State
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Applicative, Comonad, Eq, Monoid}

import proptics.internal.{Forget, Indexed, Shop, Zipping}
import proptics.newtype.Disj
import proptics.profunctor.Costar._
import proptics.profunctor.Wander._
import proptics.profunctor.{Costar, Star, Traversing, Wander}
import proptics.rank2types.Rank2TypeIndexedLensLike
import proptics.syntax.costar._
import proptics.syntax.star._
import proptics.syntax.tuple._

/** [[IndexedLens_]] is An indexed optic constrained with Strong profunctor
  *
  * @tparam I the index of an [[IndexedLens_]]
  * @tparam S the source of an [[IndexedLens_]]
  * @tparam T the modified source of an [[IndexedLens_]]
  * @tparam A the focus of an [[IndexedLens_]]
  * @tparam B the modified focus of an [[IndexedLens_]]
  */
abstract class IndexedLens_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T]

  /** view the focus and the index of an [[IndexedLens_]] */
  def view(s: S): (A, I) = self[Forget[(A, I), *, *]](Indexed(Forget(identity))).runForget(s)

  /** set the modified focus of an [[IndexedLens_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[IndexedLens_]] using a function, resulting in a change of type to the full structure */
  def over(f: ((A, I)) => B): S => T = self(Indexed(f))

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: ((A, I)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of a [[IndexedLens_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]: Applicative](s: S)(f: ((A, I)) => F[B]): F[T] = self(Indexed(Star(f))).runStar(s)

  /** test whether a predicate holds for the focus of an [[IndexedLens_]] */
  def exists(f: ((A, I)) => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of an [[IndexedLens_]] */
  def notExists(f: ((A, I)) => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether a focus at specific index of an [[IndexedLens_]] contains a given value */
  def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedLens_]] does not contain a given value */
  def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)

  /** find if a focus of an [[IndexedLens_]] that satisfies a predicate. */
  def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = s => view(s).some.filter(f)

  /** view the focus and the index of an [[IndexedLens_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, (A, I)] = ev.inspect(view)

  /** try to map a function over this [[IndexedLens_]], failing if the [[IndexedLens_]] has no foci. */
  def failover[F[_]](s: S)(f: ((A, I)) => B)(implicit ev0: Strong[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] = {
    val star = Star[(Disj[Boolean], *), (A, I), B](ia => (Disj(true), f(ia)))

    self(Indexed(star)).runStar(s) match {
      case (Disj(true), x) => ev1.pure(x)
      case (Disj(false), _) => ev1.empty
    }
  }

  /** zip two sources of a [[IndexedLens_]] together provided a binary operation which modify the focus type of a [[IndexedLens_]] */
  def zipWith[F[_]](s1: S, s2: S)(f: ((A, I), (A, I)) => B): T = self(Indexed(Zipping(f.curried))).runZipping(s1)(s2)

  /** modify an effectual focus of an [[IndexedLens_]] into the modified focus, resulting in a change of type to the full structure */
  def cotraverse[F[_]: Comonad](fs: F[S])(f: F[(A, I)] => B)(implicit ev: Applicative[F]): T = self(Indexed(Costar(f))).runCostar(fs)

  /** synonym for [[cotraverse]], flipped */
  def zipWithF[F[_]: Comonad](f: F[(A, I)] => B)(fs: F[S]): T = self(Indexed(Costar(f))).runCostar(fs)

  /** synonym to [[asLens]] */
  def unindex: Lens_[S, T, A, B] = asLens

  /** remap the index, resulting in a change of type to the full structure */
  def reindex[J](f: I => J): IndexedLens_[J, S, T, A, B] = new IndexedLens_[J, S, T, A, B] {
    override private[proptics] def apply[P[_, _]](indexed: Indexed[P, J, A, B])(implicit ev: Strong[P]): P[S, T] =
      self(indexed.reindex[I](f)(ev))
  }

  /** transform an [[IndexedLens_]] to a [[Lens_]] */
  def asLens: Lens_[S, T, A, B] = new Lens_[S, T, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] =
      self(Indexed(ev.lmap[A, B, (A, I)](pab)(_._1)))
  }

  /** compose [[IndexedLens_]] with an [[IndexedLens_]] */
  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedLens_[I, S, T, C, D] = new IndexedLens_[I, S, T, C, D] {
    override private[proptics] def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Strong[P]): P[S, T] =
      self(Indexed(ev.lmap[A, B, (A, I)](other(indexed))(_._1)))
  }

  /** compose [[IndexedLens_]] with an [[AnIndexedLens_]] */
  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    override def apply(indexed: Indexed[Shop[(C, I), D, *, *], I, C, D]): Shop[(C, I), D, S, T] =
      Shop(other.view _ compose Tuple2._1[A, I] compose self.view, s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose [[IndexedLens_]] with a [[IndexedTraversal_]] */
  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, I), D] = new Traversing[S, T, (C, I), D] {
        override def apply[F[_]](f: ((C, I)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, _) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose [[IndexedLens_]] with a [[IndexedSetter_]] */
  def compose[C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B](other(indexed) compose Tuple2._1))
  }

  /** compose [[IndexedLens_]] with a [[IndexedGetter_]] */
  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(other.view _ compose Tuple2._1[A, I] compose self.view)
  }

  /** compose [[IndexedLens_]] with a [[IndexedFold_]] */
  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1)(indexed.runIndex.runForget))
  }
}

object IndexedLens_ {
  /** create a polymorphic [[IndexedLens_]] from Rank2TypeIndexedLensLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): IndexedLens_[I, S, T, A, B] = new IndexedLens_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T] = f(indexed.runIndex)
  }

  /** create a polymorphic [[IndexedLens_]] from a getter/setter pair */
  def apply[I, S, T, A, B](get: S => (A, I))(set: S => B => T): IndexedLens_[I, S, T, A, B] =
    IndexedLens_.lens((get, set).mapN(Tuple2.apply))

  /** create a polymorphic [[IndexedLens_]] from a combined getter/setter */
  def lens[I, S, T, A, B](to: S => ((A, I), B => T)): IndexedLens_[I, S, T, A, B] =
    IndexedLens_(new Rank2TypeIndexedLensLike[I, S, T, A, B] {
      override def apply[P[_, _]](piab: P[(A, I), B])(implicit ev: Strong[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(piab)
    })

  /** lifts a combined getter/setter function to a general optic using [[Strong]] profunctor */
  private[proptics] def liftIndexedOptic[P[_, _], I, S, T, A, B](to: S => ((A, I), B => T))(implicit ev: Strong[P]): P[(A, I), B] => P[S, T] =
    piab => ev.dimap(ev.first[(A, I), B, B => T](piab))(to) { case (b, b2t) => b2t(b) }
}

object IndexedLens {
  /** create a monomorphic [[IndexedLens]] from a getter/setter pair */
  def apply[I, S, A](get: S => (A, I))(set: S => A => S): IndexedLens[I, S, A] = IndexedLens_(get)(set)

  /** create a monomorphic [[IndexedLens]] from a combined getter/setter */
  def lens[I, S, A](to: S => ((A, I), A => S)): IndexedLens[I, S, A] = IndexedLens_.lens(to)
}
