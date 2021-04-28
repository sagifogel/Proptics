package proptics

import scala.Function.const

import cats.arrow.Strong
import cats.data.State
import cats.syntax.apply._
import cats.syntax.bifunctor._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Applicative, Comonad, Eq, Monoid}

import proptics.IndexedTraversal_.wander
import proptics.data.Disj
import proptics.internal.{Forget, Indexed, Shop, Zipping}
import proptics.profunctor.Costar._
import proptics.profunctor.Wander._
import proptics.profunctor.{Costar, Star}
import proptics.rank2types.{LensLikeWithIndex, Rank2TypeIndexedLensLike}
import proptics.syntax.costar._
import proptics.syntax.star._
import proptics.syntax.tuple._

/** An [[IndexedLens_]]] focuses a single piece of data and index within a larger structure.
  *
  * An [[IndexedLens_]] provides a convenient way to view, set, and transform that element.
  *
  * An [[IndexedLens_]] must never fail to get or modify that focus.
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
  final def view(s: S): (A, I) = self[Forget[(A, I), *, *]](Indexed(Forget(identity))).runForget(s)

  /** set the modified focus of an [[IndexedLens_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[IndexedLens_]] using a function, resulting in a change of type to the full structure */
  final def over(f: ((A, I)) => B): S => T = self(Indexed(f))

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: ((A, I)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[IndexedLens_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  final def traverse[F[_]: Applicative](s: S)(f: ((A, I)) => F[B]): F[T] = self(Indexed(Star(f))).runStar(s)

  /** test whether a predicate holds for the focus of an [[IndexedLens_]] */
  final def exists(f: ((A, I)) => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of an [[IndexedLens_]] */
  final def notExists(f: ((A, I)) => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether a focus at specific index of an [[IndexedLens_]] contains a given value */
  final def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedLens_]] does not contain a given value */
  final def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)

  /** find if a focus of an [[IndexedLens_]] that satisfies a predicate */
  final def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = s => view(s).some.filter(f)

  /** view the focus and the index of an [[IndexedLens_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, (A, I)] = ev.inspect(view)

  /** try to map a function over this [[IndexedLens_]], failing if the [[IndexedLens_]] has no foci. */
  final def failover[F[_]](s: S)(f: ((A, I)) => B)(implicit ev0: Strong[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] = {
    val star = Star[(Disj[Boolean], *), (A, I), B](ia => (Disj(true), f(ia)))

    self(Indexed(star)).runStar(s) match {
      case (Disj(true), x) => ev1.pure(x)
      case (Disj(false), _) => ev1.empty
    }
  }

  /** zip two sources of an [[IndexedLens_]] together provided a binary operation which modify the focus type of an [[IndexedLens_]] */
  final def zipWith[F[_]](s1: S, s2: S)(f: ((A, I), (A, I)) => B): T = self(Indexed(Zipping(f.curried))).runZipping(s1)(s2)

  /** modify an effectual focus of an [[IndexedLens_]] into the modified focus, resulting in a change of type to the full structure */
  final def cotraverse[F[_]: Comonad](fs: F[S])(f: F[(A, I)] => B)(implicit ev: Applicative[F]): T = self(Indexed(Costar(f))).runCostar(fs)

  /** synonym for [[cotraverse]], flipped */
  final def zipWithF[F[_]: Comonad](f: F[(A, I)] => B)(fs: F[S]): T = self(Indexed(Costar(f))).runCostar(fs)

  /** synonym to [[asLens]] */
  final def unindex: Lens_[S, T, A, B] = asLens

  /** remap the index, resulting in a change of type to the full structure */
  final def reindex[J](f: I => J): IndexedLens_[J, S, T, A, B] = new IndexedLens_[J, S, T, A, B] {
    override private[proptics] def apply[P[_, _]](indexed: Indexed[P, J, A, B])(implicit ev: Strong[P]): P[S, T] =
      self(indexed.reindex[I](f)(ev))
  }

  /** transform an [[IndexedLens_]] to a [[Lens_]] */
  final def asLens: Lens_[S, T, A, B] = new Lens_[S, T, A, B] {
    override private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] =
      self(Indexed(ev.lmap[A, B, (A, I)](pab)(_._1)))
  }

  /** transform an [[IndexedLens_]] to an [[IndexedFold_]] */
  final def asIndexedFold: IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose self.view)
  }

  /** compose an [[IndexedLens_]] with an [[Iso_]] */
  final def compose[C, D](other: Iso_[A, B, C, D]): IndexedLens_[I, S, T, C, D] =
    IndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with an [[AnIso_]] */
  final def compose[C, D](other: AnIso_[A, B, C, D]): IndexedLens_[I, S, T, C, D] =
    IndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with a [[Lens_]] */
  final def compose[C, D](other: Lens_[A, B, C, D]): IndexedLens_[I, S, T, C, D] =
    IndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with an [[ALens_]] */
  final def compose[C, D](other: ALens_[A, B, C, D]): IndexedLens_[I, S, T, C, D] =
    IndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with a [[Prism_]] */
  final def compose[C, D](other: Prism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with an [[APrism_]] */
  final def compose[C, D](other: APrism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with an [[AffineTraversal_]] */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with an [[AnAffineTraversal_]] */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with a [[Traversal_]] */
  final def compose[C, D](other: Traversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedLens_]] with an [[ATraversal_]] */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with a [[Setter_]] */
  final def compose[C, D](other: Setter_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) }
  }

  /** compose an [[IndexedTraversal_]] with a [[Getter_]] */
  final def compose[C, D](other: Getter_[A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        indexed.runIndex.runForget((other.view(a), i))
      }
  }

  /** compose an [[IndexedTraversal_]] with a [[Fold_]] */
  final def compose[C, D](other: Fold_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget { s =>
        val (a, i) = self.view(s)
        other.foldMap(a)(c => indexed.runIndex.runForget((c, i)))
      }
  }

  /** compose an [[IndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedLens_[J, S, T, C, D] = new IndexedLens_[J, S, T, C, D] {
    override private[proptics] def apply[P[_, _]](indexed: Indexed[P, J, C, D])(implicit ev: Strong[P]): P[S, T] =
      self(Indexed(ev.lmap[A, B, (A, I)](other(indexed))(_._1)))
  }

  /** compose an [[IndexedLens_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedLens_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedLens_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedLens_[I, S, T, C, D] =
    IndexedLens_[I, S, T, C, D]((s: S) => self.view(s).leftMap(other.view(_)._1)) { s => d =>
      self.set(other.set(d)(self.view(s)._1))(s)
    }

  /** compose an [[IndexedLens_]] with an [[IndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedLens_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = new AnIndexedLens_[J, S, T, C, D] {
    override def apply(indexed: Indexed[Shop[(C, J), D, *, *], J, C, D]): Shop[(C, J), D, S, T] =
      Shop[(C, J), D, S, T](s => other.view(self.view(s)._1), s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[IndexedLens_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): AnIndexedLens_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedLens_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = new AnIndexedLens_[I, S, T, C, D] {
    override def apply(indexed: Indexed[Shop[(C, I), D, *, *], I, C, D]): Shop[(C, I), D, S, T] =
      Shop[(C, I), D, S, T](s => self.view(s).leftMap(other.view(_)._1), s => d => self.set(other.set(d)(self.view(s)._1))(s))
  }

  /** compose an [[IndexedLens_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: AnIndexedLens_[_, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[J, S, T, C, D] {
      override def apply[F[_]](f: ((C, J)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, _) => other.overF(f)(a) }
    })

  /** compose an [[IndexedLens_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF { case (a, i) => other.overF { case (c, _) => f((c, i)) }(a) }
    })

  /** compose an [[IndexedLens_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T =
      self(Indexed[* => *, I, A, B](other(indexed) compose Tuple2._1))
  }

  /** compose an [[IndexedLens_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedSetter_]], while preserving self indices */
  final def composeWithLeftIndex[J, C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B] { case (a, i) => other.over { case (c, _) => indexed.runIndex((c, i)) }(a) })
  }

  /** compose an [[IndexedLens_]] with an [[IndexedSetter_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = new IndexedGetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, J), *, *], J, C, D]): Forget[(C, J), S, T] =
      Forget(other.view _ compose Tuple2._1[A, I] compose self.view)
  }

  /** compose an [[IndexedLens_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedGetter_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedGetter_]], while preserving self indices */
  final def composeWithLeftIndex[J, C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(C, I), *, *], I, C, D]): Forget[(C, I), S, T] =
      Forget(s => self.view(s).leftMap(other.view(_)._1))
  }

  /** compose an [[IndexedLens_]] with an [[IndexedGetter_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1)(indexed.runIndex.runForget))
  }

  /** compose an [[IndexedLens_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedLens_]] with an [[IndexedFold_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._1) { case (c, _) => indexed.runIndex.runForget((c, self.view(s)._2)) })
  }

  /** compose an [[IndexedLens_]] with an [[IndexedFold_]], while preserving self indices */
  final def <<*[J, C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  private def composeWithTraverseFn[F[_]: Applicative, C, D](f: ((C, I)) => F[D])(g: (C => F[D]) => A => F[B]): S => F[T] =
    self.overF { case (a, i) => g(c => f((c, i)))(a) }
}

object IndexedLens_ {
  /** create a polymorphic [[IndexedLens_]] from Rank2TypeIndexedLensLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): IndexedLens_[I, S, T, A, B] = new IndexedLens_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T] = f(indexed.runIndex)
  }

  /** create a polymorphic [[IndexedLens_]] from a getter/setter pair */
  final def apply[I, S, T, A, B](get: S => (A, I))(set: S => B => T): IndexedLens_[I, S, T, A, B] =
    IndexedLens_.lens((get, set).mapN(Tuple2.apply))

  /** create a polymorphic [[IndexedLens_]] from a combined getter/setter */
  final def lens[I, S, T, A, B](to: S => ((A, I), B => T)): IndexedLens_[I, S, T, A, B] =
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
  final def apply[I, S, A](get: S => (A, I))(set: S => A => S): IndexedLens[I, S, A] = IndexedLens_(get)(set)

  /** create a monomorphic [[IndexedLens]] from a combined getter/setter */
  final def lens[I, S, A](to: S => ((A, I), A => S)): IndexedLens[I, S, A] = IndexedLens_.lens(to)
}
