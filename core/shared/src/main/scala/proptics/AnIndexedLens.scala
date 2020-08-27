package proptics

import cats.arrow.Strong
import cats.data.State
import cats.instances.function._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid}
import proptics.IndexedLens_.liftIndexedOptic
import proptics.internal._
import proptics.rank2types.{Rank2TypeIndexedLensLike, Traversing}
import proptics.syntax.tuple._

import scala.Function.const

/**
  * An [[IndexedLens_]] with fixed type [[Shop [[cats.arrow.Profunctor]]
  *
  * @tparam I the index of an [[AnIndexedLens_]]
  * @tparam S the source of an [[AnIndexedLens_]]
  * @tparam T the modified source of an [[AnIndexedLens_]]
  * @tparam A the focus of an [[AnIndexedLens_]]
  * @tparam B the modified focus of an [[AnIndexedLens_]]
  */
abstract class AnIndexedLens_[I, S, T, A, B] { self =>
  def apply(indexed: Indexed[Shop[(I, A), B, *, *], I, A, B]): Shop[(I, A), B, S, T]

  /** view the focus and the index of an [[AnIndexedLens_]] */
  def view(s: S): (I, A) = applyShop.get(s)

  /** set the modified focus of an [[AnIndexedLens_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[AnIndexedLens_]] using a function, resulting in a change of type to the full structure  */
  def over(f: ((I, A)) => B): S => T = overF[Id](f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: ((I, A)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AnIndexedLens_]] using a [[cats.Functor]], resulting in a change of type to the full structure  */
  def traverse[F[_]](s: S)(f: ((I, A)) => F[B])(implicit ev: Applicative[F]): F[T] = {
    val shop = applyShop
    ev.map(f(shop.get(s)))(shop.set(s)(_))
  }

  /** test whether a predicate holds for the focus of an [[AnIndexedLens_]] */
  def exists(f: ((I, A)) => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of an [[AnIndexedLens_]] */
  def notExists(f: ((I, A)) => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether a focus at specific index of an [[AnIndexedLens_]] contains a given value */
  def contains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[AnIndexedLens_]] does not contain a given value */
  def notContains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = !contains(s)(a)

  /** find if a focus of an [[AnIndexedLens_]] that satisfies a predicate. */
  def find(f: ((I, A)) => Boolean): S => Option[A] = s => view(s).some.filter(f).map(_._2)

  /** view the focus and the index of an [[AnIndexedLens_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, (I, A)] = ev.inspect(view)

  /** convert an [[AnIndexedLens_]] to the pair of functions that characterize it */
  def withIndexedLens[R](f: (S => (I, A)) => (S => B => T) => R): R = {
    val shop = applyShop

    f(shop.get)(shop.set)
  }

  /** transform an [[AnIndexedLens_]] to an [[IndexedLens_]] */
  def asIndexedLens: IndexedLens_[I, S, T, A, B] = withIndexedLens(IndexedLens_[I, S, T, A, B])

  /** synonym to [[asLens]] */
  def unindex: Lens_[S, T, A, B] = asLens

  /** transform an [[AnIndexedLens_]] to an [[Lens_]] */
  def asLens: Lens_[S, T, A, B] = withIndexedLens(sia => sbt => Lens_.lens(s => (sia(s)._2, sbt(s))))

  /** compose [[AnIndexedLens_]] with an [[IndexedLens_]] */
  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(I, C), D, *, *], I, C, D]): Shop[(I, C), D, S, T] =
      Shop(other.view _ compose Tuple2._2[I, A] compose self.view, s => d => self.set(other.set(d)(self.view(s)._2))(s))
  }

  /** compose [[AnIndexedLens_]] with an [[AnIndexedLens_]] */
  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(I, C), D, *, *], I, C, D]): Shop[(I, C), D, S, T] =
      Shop(other.view _ compose Tuple2._2[I, A] compose self.view, s => d => self.set(other.set(d)(self.view(s)._2))(s))
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedTraversal_]] */
  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (I, C), D] = new Traversing[S, T, (I, C), D] {
        override def apply[F[_]](f: ((I, C)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (_, a) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedSetter_]] */
  def compose[C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.traverse[Id](_)(other(indexed) compose Tuple2._2)
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedGetter_]] */
  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(I, C), *, *], I, C, D]): Forget[(I, C), S, T] =
      Forget(other.view _ compose Tuple2._2[I, A] compose self.view)
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedFold_]] */
  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other(indexed).runForget(self.view(s)._2))
  }

  private def applyShop: Shop[(I, A), B, S, T] = self(Indexed(Shop(identity, const(identity))))
}

object AnIndexedLens_ {

  /** create a polymorphic [[AnIndexedLens_]] from Rank2TypeIndexedLensLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): AnIndexedLens_[I, S, T, A, B] = new AnIndexedLens_[I, S, T, A, B] {
    override def apply(indexed: Indexed[Shop[(I, A), B, *, *], I, A, B]): Shop[(I, A), B, S, T] =
      f(indexed.runIndex)
  }

  /** create a polymorphic [[AnIndexedLens_]] from a getter/setter pair */
  def apply[I, S, T, A, B](get: S => (I, A))(_set: S => B => T): AnIndexedLens_[I, S, T, A, B] =
    AnIndexedLens_.lens((get, _set).mapN(Tuple2.apply))

  /** create a polymorphic [[AnIndexedLens_]] from a combined getter/setter */
  def lens[I, S, T, A, B](to: S => ((I, A), B => T)): AnIndexedLens_[I, S, T, A, B] =
    AnIndexedLens_(new Rank2TypeIndexedLensLike[I, S, T, A, B] {
      override def apply[P[_, _]](piab: P[(I, A), B])(implicit ev: Strong[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(piab)
    })
}

object AnIndexedLens {

  /** create a monomorphic [[AnIndexedLens]] from a getter/setter pair */
  def apply[I, S, A](get: S => (I, A))(set: S => A => S): AnIndexedLens[I, S, A] = AnIndexedLens_(get)(set)

  /** create a monomorphic [[AnIndexedLens]] from a combined getter/setter */
  def lens[I, S, A](to: S => ((I, A), A => S)): AnIndexedLens[I, S, A] = AnIndexedLens_.lens(to)
}
