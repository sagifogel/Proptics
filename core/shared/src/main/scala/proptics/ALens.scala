package proptics

import scala.Function.const

import cats.data.State
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Functor, Id, Monoid}

import proptics.internal.{Forget, Indexed, RunBazaar, Shop}
import proptics.profunctor.{Traversing, Wander}
import proptics.rank2types.LensLikeWithIndex

/** An [[ALens_]]] focuses a single piece of data within a larger structure.
  *
  * An [[ALens_]] provides a convenient way to view, set, and transform that element.
  *
  * An [[ALens_]] must never fail to get or modify that focus.
  *
  * An [[ALens_]] is a [[Lens_]] with fixed type [[proptics.internal.Shop]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of a [[ALens_]]
  * @tparam T the modified source of a [[ALens_]]
  * @tparam A the focus of a [[ALens_]]
  * @tparam B the modified focus of a [[ALens_]]
  */
abstract class ALens_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(shop: Shop[A, B, A, B]): Shop[A, B, S, T]

  /** view the focus of a [[ALens_]] */
  final def view(s: S): A = toShop.view(s)

  /** set the modified focus of a [[ALens_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the focus type of a [[ALens_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = s => overF[Id](f)(s)

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Functor](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of a [[ALens_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  final def traverse[F[_]: Functor](s: S)(f: A => F[B])(implicit ev: Functor[F]): F[T] = {
    val shop: Shop[A, B, S, T] = toShop

    ev.map(f(shop.view(s)))(shop.set(s))
  }

  /** test whether a predicate holds for the focus of a [[ALens_]] */
  final def exists(f: A => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of a [[ALens_]] */
  final def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of a [[ALens_]] contains a given value */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus a [[ALens_]] does not contain a given value */
  final def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** find if the focus of a [[ALens_]] is satisfying a predicate. */
  final def find(f: A => Boolean): S => Option[A] = s => view(s).some.filter(f)

  /** view the focus of an [[ALens_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, A] = ev.inspect(view)

  /** convert an [[ALens_]] to the pair of functions that characterize it */
  final def withLens[R](f: (S => A) => (S => B => T) => R): R = {
    val shop = toShop

    f(shop.view)(shop.set)
  }

  /** convert an [[ALens_]] to a Shop[A, B, S, T] */
  final def toShop: Shop[A, B, S, T] = self(Shop(identity, const(identity)))

  /** transform an [[ALens_]] to a [[Lens_]] */
  final def asLens: Lens_[S, T, A, B] = withLens(Lens_[S, T, A, B])

  /** transform an [[ALens_]] to a [[Lens_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    final override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(forget.runForget compose self.view)
  }

  /** convert an [[ALens_]] into the form that a [[Lens_]] accepts.
    *
    * Can be useful when final defining a lens where the focus appears under multiple
    * constructors of an algebraic data type. This function would be called for
    * each case of the data type.
    */
  final def lensStore(s: S): (A, B => T) = withLens(sa => sbt => (sa, sbt).mapN(Tuple2.apply))(s)

  /** compose a [[ALens_]] with a function lifted to a [[Getter_]] */
  final def to[C, D](f: A => C): Getter_[S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose an [[ALens_]] with an [[Iso_]] */
  final def compose[C, D](other: Iso_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    final override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] = self.toShop compose other(shop)
  }

  /** compose an [[ALens_]] with an [[AnIso_]] */
  final def compose[C, D](other: AnIso_[A, B, C, D]): ALens_[S, T, C, D] = self compose other.asIso

  /** compose an [[ALens_]] with an [[Lens_]] */
  final def compose[C, D](other: Lens_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    final override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] = self.toShop compose other(shop)
  }

  /** compose an [[ALens_]] with an [[ALens_]] */
  final def compose[C, D](other: ALens_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    final override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] = self.toShop compose other(shop)
  }

  /** compose a [[Lens_]] with a [[Prism_]] */
  final def compose[C, D](other: Prism_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self.asLens compose other

  /** compose an [[ALens_]] with an [[APrism_]] */
  final def compose[C, D](other: APrism_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self.asLens compose other

  /** compose an [[ALens_]] with an [[AffineTraversal_]] */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): AffineTraversal_[S, T, C, D] =
    AffineTraversal_ { s: S => other.viewOrModify(self.view(s)).leftMap(self.set(_)(s)) }(s => d => self.over(other.set(d))(s))

  /** compose an [[ALens_]] with an [[AnAffineTraversal_]] */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      other.viewOrModify(self.view(s)).leftMap(self.set(_)(s))
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[ALens_]] with an [[Traversal_]] */
  final def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    final override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        final override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pab)
    }
  }

  /** compose an [[ALens_]] with an [[ATraversal_]] */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      final override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[ALens_]] with an [[Setter_]] */
  final def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    final override private[proptics] def apply(pab: C => D): S => T = s => {
      val shop = toShop

      shop.set(s)(other(pab)(shop.view(s)))
    }
  }

  /** compose an [[ALens_]] with an [[Getter_]] */
  final def compose[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    final override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  /** compose an [[ALens_]] with an [[Fold_]] */
  final def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    final override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(forget.runForget))
  }

  /** compose an [[ALens_]] with an [[IndexedLens_]] */
  final def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedLens_[I, S, T, C, D] =
    IndexedLens_[I, S, T, C, D]((s: S) => other.view(self.view(s)))(s => d => self.set(other.set(d)(self.view(s)))(s))

  /** compose an [[ALens_]] with an [[AnIndexedLens_]] */
  final def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): AnIndexedLens_[I, S, T, C, D] =
    AnIndexedLens_[I, S, T, C, D]((s: S) => other.view(self.view(s)))(s => d => self.set(other.set(d)(self.view(s)))(s))

  /** compose a [[ALens_]] with an [[IndexedTraversal_]] */
  final def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    IndexedTraversal_.wander(new LensLikeWithIndex[I, S, T, C, D] {
      final override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.traverse(_)(other.traverse(_)(f))
    })

  /** compose an [[ALens_]] with an [[IndexedSetter_]] */
  final def compose[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    final override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T = s => self.set(other.over { case (c, i) => indexed.runIndex((c, i)) }(self.view(s)))(s)
  }

  /** compose an [[ALens_]] with an [[IndexedGetter_]] */
  final def compose[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    final override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose other.view compose self.view)
  }

  /** compose an [[ALens_]] with an [[IndexedFold_]] */
  final def compose[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    final override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(indexed.runIndex.runForget))
  }
}

object ALens_ {
  /** create a polymorphic [[ALens_]] from Rank2TypeLensLike encoding */
  final private[proptics] def apply[S, T, A, B](f: Shop[A, B, A, B] => Shop[A, B, S, T]): ALens_[S, T, A, B] = new ALens_[S, T, A, B] { self =>
    final override def apply(shop: Shop[A, B, A, B]): Shop[A, B, S, T] = f(shop)
  }

  /** create a polymorphic [[ALens_]] from a getter/setter pair */
  final def apply[S, T, A, B](get: S => A)(set: S => B => T): ALens_[S, T, A, B] = ALens_.lens(get)(set)

  /** create a polymorphic [[ALens_]] from a getter/setter pair */
  final def lens[S, T, A, B](get: S => A)(set: S => B => T): ALens_[S, T, A, B] =
    ALens_ { shop =>
      Shop(
        shop.view compose get,
        s =>
          b => {
            val a = get(s)

            set(s)(shop.set(a)(b))
          })
    }

  /** polymorphic identity of an [[ALens_]] */
  final def id[S, T]: ALens_[S, T, S, T] = ALens_(identity[S] _)(const(identity[T]))

  /** use a [[Prism_]] as a kind of first-class pattern. */
  final def outside[S, T, A, B, R](prism: Prism_[S, T, A, B]): ALens_[T => R, S => R, B => R, A => R] =
    ALens_[T => R, S => R, B => R, A => R]((f: T => R) => f compose prism.review) { t2r => a2r => s =>
      prism.viewOrModify(s).fold(t2r, a2r)
    }
}

object ALens {
  /** create a monomorphic [[ALens]] from a getter/setter pair */
  final def apply[S, A](get: S => A)(set: S => A => S): ALens[S, A] = ALens_(get)(set)

  /** monomorphic identity of an [[ALens]] */
  final def id[S]: ALens[S, S] = ALens_.id[S, S]

  /** use a [[Prism]] as a kind of first-class pattern. */
  final def outside[S, A, R](aPrism: Prism[S, A]): ALens[S => R, A => R] = ALens_.outside(aPrism)
}
