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
  def view(s: S): (A, I) = toShop.view(s)

  /** set the modified focus of an [[AnIndexedLens_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[AnIndexedLens_]] using a function, resulting in a change of type to the full structure */
  def over(f: ((A, I)) => B): S => T = overF[Id](f)

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: ((A, I)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AnIndexedLens_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]](s: S)(f: ((A, I)) => F[B])(implicit ev: Applicative[F]): F[T] = {
    val shop = toShop
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
  def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = s => view(s).some.filter(f)

  /** view the focus and the index of an [[AnIndexedLens_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, (A, I)] = ev.inspect(view)

  /** convert an [[AnIndexedLens_]] to the pair of functions that characterize it */
  def withIndexedLens[R](f: (S => (A, I)) => (S => B => T) => R): R = {
    val shop = toShop

    f(shop.view)(shop.set)
  }

  /** convert an [[AnIndexedLens_]] to a Shop[(A, I), B, S, T] */
  def toShop: Shop[(A, I), B, S, T] = self(Indexed(Shop(identity, const(identity))))

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

  /** transform an [[IndexedLens_]] to an [[IndexedFold_]] */
  def asIndexedFold: IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose self.view)
  }

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

  /** compose an [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = new AnIndexedLens_[J, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(C, J), D, *, *], J, C, D]): Shop[(C, J), D, S, T] =
      Shop(other.view _ compose Tuple2._1[A, I] compose self.view, s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  def composeWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(C, I), D, *, *], I, C, D]): Shop[(C, I), D, S, T] =
      Shop(self.view(_).leftMap(other.view(_)._1), s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedLens_]], while preserving self indices */
  def <<*[C, D](other: IndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = new AnIndexedLens_[J, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(C, J), D, *, *], J, C, D]): Shop[(C, J), D, S, T] =
      Shop(other.view _ compose Tuple2._1[A, I] compose self.view, s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  def composeWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    def apply(indexed: Indexed[Shop[(C, I), D, *, *], I, C, D]): Shop[(C, I), D, S, T] =
      Shop(self.view(_).leftMap(other.view(_)._1), s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[AnIndexedLens_]] with an [[AnIndexedLens_]], while preserving self indices */
  def <<*[C, D](other: AnIndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[J, S, T, C, D] {
      override def apply[F[_]](f: ((C, J)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, _) => other.overF(f)(a) }
    })

  /** compose an [[AnIndexedLens_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedTraversal_]], while preserving self indices */
  def composeWithLeftIndex[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF { case (c, _) => f((c, i)) }(a) }
    })

  /** compose an [[AnIndexedLens_]] with an [[IndexedTraversal_]], while preserving self indices */
  def <<*[J, C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T = s => self.set(other.over(indexed.runIndex)(self.view(s)._1))(s)
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedSetter_]], while preserving self indices */
  def composeWithLeftIndex[J, C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T = s => {
      val (a, i) = self.view(s)

      self.set(other.over { case (c, _) => indexed.runIndex((c, i)) }(a))(s)
    }
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedSetter_]], while preserving self indices */
  def <<*[J, C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = new IndexedGetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, J), *, *], J, C, D]): Forget[(C, J), S, T] =
      Forget(other.view _ compose Tuple2._1[A, I] compose self.view)
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedGetter_]], while preserving self indices */
  def composeWithLeftIndex[J, C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(s => self.view(s).leftMap(other.view(_)._1))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedGetter_]], while preserving self indices */
  def <<*[J, C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1)(indexed.runIndex.runForget))
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[AnIndexedLens_]] with an [[IndexedFold_]], while preserving self indices */
  def composeWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1) { case (c, _) => indexed.runIndex.runForget((c, self.view(s)._2)) })
  }

  /** compose an [[AnIndexedLens_]] with an [[IndexedFold_]], while preserving self indices */
  def <<*[J, C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

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
