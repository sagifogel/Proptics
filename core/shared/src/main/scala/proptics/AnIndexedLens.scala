package proptics

import scala.Function.const

import cats.arrow.Strong
import cats.data.State
import cats.syntax.apply._
import cats.syntax.bifunctor._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid}

import proptics.IndexedLens_.liftIndexedOptic
import proptics.IndexedTraversal_.wander
import proptics.internal._
import proptics.profunctor.{Traversing, Wander}
import proptics.rank2types.{LensLikeWithIndex, Rank2TypeIndexedLensLike}
import proptics.syntax.tuple._

/** [[AnIndexedLens_]] is an [[IndexedLens_]] with fixed type [[Shop]] Profunctor
  *
  * @tparam I the index of an [[AnIndexedLens_]]
  * @tparam S the source of an [[AnIndexedLens_]]
  * @tparam T the modified source of an [[AnIndexedLens_]]
  * @tparam A the focus of an [[AnIndexedLens_]]
  * @tparam B the modified focus of an [[AnIndexedLens_]]
  */
abstract class AnIndexedLens_[I, S, T, A, B] { self =>
  def apply(indexed: Indexed[Shop[(A, I), B, *, *], I, A, B]): Shop[(A, I), B, S, T]

  /** view the focus and the index of an [[AnIndexedLens_]] */
  def view(s: S): (A, I) = applyShop.view(s)

  /** set the modified focus of an [[AnIndexedLens_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[AnIndexedLens_]] using a function, resulting in a change of type to the full structure */
  def over(f: ((A, I)) => B): S => T = overF[Id](f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: ((A, I)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AnIndexedLens_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]](s: S)(f: ((A, I)) => F[B])(implicit ev: Applicative[F]): F[T] = {
    val shop = applyShop
    ev.map(f(shop.view(s)))(shop.set(s)(_))
  }

  /** test whether a predicate holds for the focus of an [[AnIndexedLens_]] */
  def exists(f: ((A, I)) => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of an [[AnIndexedLens_]] */
  def notExists(f: ((A, I)) => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether a focus at specific index of an [[AnIndexedLens_]] contains a given value */
  def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[AnIndexedLens_]] does not contain a given value */
  def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)

  /** find if a focus of an [[AnIndexedLens_]] that satisfies a predicate. */
  def find(f: ((A, I)) => Boolean): S => Option[A] = s => view(s).some.filter(f).map(_._1)

  /** view the focus and the index of an [[AnIndexedLens_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, (A, I)] = ev.inspect(view)

  /** convert an [[AnIndexedLens_]] to the pair of functions that characterize it */
  def withIndexedLens[R](f: (S => (A, I)) => (S => B => T) => R): R = {
    val shop = applyShop

    f(shop.view)(shop.set)
  }

  /** transform an [[AnIndexedLens_]] to an [[IndexedLens_]] */
  def asIndexedLens: IndexedLens_[I, S, T, A, B] = withIndexedLens(IndexedLens_[I, S, T, A, B])

  /** synonym to [[asLens]] */
  def unindex: Lens_[S, T, A, B] = asLens

  /** remap the index, resulting in a change of type to the full structure */
  def reindex[J](f: I => J): AnIndexedLens_[J, S, T, A, B] = new AnIndexedLens_[J, S, T, A, B] {
    override def apply(indexed: Indexed[Shop[(A, J), B, *, *], J, A, B]): Shop[(A, J), B, S, T] = {
      val shop: Shop[(A, J), B, (A, I), B] = indexed.reindex[I](f).runIndex

      Shop(shop.view compose self.view, s => b => self.set(shop.set(self.view(s))(b))(s))
    }
  }

  /** transform an [[AnIndexedLens_]] to an [[Lens_]] */
  def asLens: Lens_[S, T, A, B] = withIndexedLens(sia => sbt => Lens_.lens(s => (sia(s)._1, sbt(s))))

  /** compose an [[IndexedLens_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with an [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with an [[AnAffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, (C, I), D] = new Setter_[S, T, (C, I), D] {
    override private[proptics] def apply(pab: ((C, I)) => D): S => T =
      self.over { case (a, i) => other.over(c => pab((c, i)))(a) }
  }

  /** compose an [[IndexedTraversal_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, (C, I), D] = new Getter_[S, T, (C, I), D] {
    override private[proptics] def apply(forget: Forget[(C, I), (C, I), D]): Forget[(C, I), S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        forget.runForget((other.view(a), i))
      }
  }

  /** compose an [[IndexedTraversal_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, (C, I), D] = new Fold_[S, T, (C, I), D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, (C, I), D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.foldMap(a)(c => forget.runForget((c, i)))
      }
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = new AnIndexedLens_[J, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(C, J), D, *, *], J, C, D]): Shop[(C, J), D, S, T] =
      Shop(other.view _ compose Tuple2._1[A, I] compose self.view, s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  def composeWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(C, I), D, *, *], I, C, D]): Shop[(C, I), D, S, T] =
      Shop(self.view(_).leftMap(other.view(_)._1), s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose [[IndexedLens_]] with an [[IndexedLens_]], while preserving self indices */
  def <<*[C, D](other: IndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = new AnIndexedLens_[J, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(C, J), D, *, *], J, C, D]): Shop[(C, J), D, S, T] =
      Shop(other.view _ compose Tuple2._1[A, I] compose self.view, s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  def composeWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(C, I), D, *, *], I, C, D]): Shop[(C, I), D, S, T] =
      Shop(self.view(_).leftMap(other.view(_)._1), s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose [[IndexedLens_]] with an [[AnIndexedLens_]], while preserving self indices */
  def <<*[C, D](other: AnIndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose [[AnIndexedLens_]] with an [[IndexedTraversal_]] */
  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, I), D] = new Traversing[S, T, (C, I), D] {
        override def apply[F[_]](f: ((C, I)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, _) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedSetter_]] */
  def compose[C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.traverse[Id](_)(other(indexed) compose Tuple2._1)
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedGetter_]] */
  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(other.view _ compose Tuple2._1[A, I] compose self.view)
  }

  /** compose [[AnIndexedLens_]] with an [[IndexedFold_]] */
  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other(indexed).runForget(self.view(s)._1))
  }

  private def applyShop: Shop[(A, I), B, S, T] = self(Indexed(Shop(identity, const(identity))))

  private def composeWithTraverseFn[F[_]: Applicative, C, D](f: ((C, I)) => F[D])(g: (C => F[D]) => A => F[B]): S => F[T] =
    self.overF { case (a, i) => g(c => f((c, i)))(a) }
}

object AnIndexedLens_ {
  /** create a polymorphic [[AnIndexedLens_]] from Rank2TypeIndexedLensLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): AnIndexedLens_[I, S, T, A, B] = new AnIndexedLens_[I, S, T, A, B] {
    override def apply(indexed: Indexed[Shop[(A, I), B, *, *], I, A, B]): Shop[(A, I), B, S, T] =
      f(indexed.runIndex)
  }

  /** create a polymorphic [[AnIndexedLens_]] from a getter/setter pair */
  def apply[I, S, T, A, B](get: S => (A, I))(_set: S => B => T): AnIndexedLens_[I, S, T, A, B] =
    AnIndexedLens_.lens((get, _set).mapN(Tuple2.apply))

  /** create a polymorphic [[AnIndexedLens_]] from a combined getter/setter */
  def lens[I, S, T, A, B](to: S => ((A, I), B => T)): AnIndexedLens_[I, S, T, A, B] =
    AnIndexedLens_(new Rank2TypeIndexedLensLike[I, S, T, A, B] {
      override def apply[P[_, _]](piab: P[(A, I), B])(implicit ev: Strong[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(piab)
    })
}

object AnIndexedLens {
  /** create a monomorphic [[AnIndexedLens]] from a getter/setter pair */
  def apply[I, S, A](get: S => (A, I))(set: S => A => S): AnIndexedLens[I, S, A] = AnIndexedLens_(get)(set)

  /** create a monomorphic [[AnIndexedLens]] from a combined getter/setter */
  def lens[I, S, A](to: S => ((A, I), A => S)): AnIndexedLens[I, S, A] = AnIndexedLens_.lens(to)
}
