package proptics

import scala.Function.const
import scala.reflect.ClassTag

import cats.data.{Const, State}
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid, Order, Traverse}
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}
import spire.std.boolean._

import proptics.IndexedLens_.liftIndexedOptic
import proptics.IndexedTraversal_.wander
import proptics.data.{Additive, Conj, Disj, Dual, Endo, First, Last, Multiplicative}
import proptics.indices.TraverseWithIndex
import proptics.internal._
import proptics.profunctor.Wander._
import proptics.profunctor.{Star, Traversing, Wander}
import proptics.rank2types.{LensLikeWithIndex, Rank2TypeIndexedTraversalLike, Rank2TypeTraversalLike}
import proptics.syntax.function._
import proptics.syntax.indexedTraversal._
import proptics.syntax.star._
import proptics.syntax.tuple._

/** An [[IndexedTraversal_]] is an indexed optic that focuses on zero or more values, and their indices.
  *
  * @tparam I the index of an [[IndexedTraversal_]]
  * @tparam S the source of an [[IndexedTraversal_]]
  * @tparam T the modified source of an [[IndexedTraversal_]]
  * @tparam A the foci of an [[IndexedTraversal_]]
  * @tparam B the modified foci of an [[IndexedTraversal_]]
  */
abstract class IndexedTraversal_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T]

  /** collect all the foci and indices of an [[IndexedTraversal_]] into a [[List]] */
  final def viewAll(s: S): List[(A, I)] = foldMap(s)(List(_))

  /** view the first focus and index of an [[IndexedTraversal_]], if there is any */
  final def preview(s: S): Option[(A, I)] = foldMap(s)(ai => First(ai.some)).runFirst

  /** set the modified foci of an [[IndexedTraversal_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the foci type of an [[IndexedTraversal_]] using a function, resulting in a change of type to the full structure */
  final def over(f: ((A, I)) => B): S => T = self(Indexed(f))

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: ((A, I)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify each focus of an [[IndexedTraversal_]] using a Functor, resulting in a change of type to the full structure */
  final def traverse[F[_]: Applicative](s: S)(f: ((A, I)) => F[B]): F[T] =
    self[Star[F, *, *]](Indexed(Star[F, (A, I), B](f))).runStar(s)

  /** map each focus and index of an [[IndexedTraversal_]] to a Monoid, and combine the results */
  final def foldMap[R: Monoid](s: S)(f: ((A, I)) => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** fold the foci and indices of an [[IndexedTraversal_]] using a binary operator, going right to left */
  final def foldRight[R](s: S)(r: R)(f: ((A, I), R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci and indices of an [[IndexedTraversal_]] using a binary operator, going left to right */
  final def foldLeft[R](s: S)(r: R)(f: (R, (A, I)) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** evaluate each focus and index of an [[IndexedTraversal_]] from left to right, and ignore the results structure */
  final def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  /** map each focus and index of an [[IndexedTraversal_]] to an effect, from left to right, and ignore the results */
  final def traverse_[F[_], R](s: S)(f: ((A, I)) => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldLeft[F[Unit]](s)(ev.pure(()))((b, ia) => ev.void(f(ia)) *> b)

  /** the sum of all foci of an [[IndexedTraversal_]] */
  final def sum(s: S)(implicit ev: AdditiveMonoid[A]): A =
    foldMap(s)(Additive[A] _ compose Tuple2._1).runAdditive

  /** the product of all foci of an [[IndexedTraversal_]] */
  final def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A =
    foldMap(s)(Multiplicative[A] _ compose Tuple2._1).runMultiplicative

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedTraversal_]] */
  final def forall(f: ((A, I)) => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  final def forall[R: Heyting](s: S)(f: ((A, I)) => R): R = foldMap(s)(Conj[R] _ compose f).runConj

  /** return the result of a conjunction of all foci of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  final def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(_._1)

  /** return the result of a disjunction of all foci of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  final def or(s: S)(implicit ev: Heyting[A]): A = any[Id, A](s)(_._1)

  /** test whether a predicate holds for any focus and index of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  final def any[F[_], R: Heyting](s: S)(f: ((A, I)) => R): R = foldMap(s)(Disj[R] _ compose f).runDisj

  /** test whether a predicate holds for any focus and index of an [[IndexedTraversal_]], using a [[Heyting]] algebra */
  final def exists(f: ((A, I)) => Boolean): S => Boolean = s => any[Disj, Boolean](s)(f)

  /** test whether a predicate does not hold for any focus and index of an [[IndexedTraversal_]] */
  final def notExists(f: ((A, I)) => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a focus at specific index of an [[IndexedTraversal_]] contains a given value */
  final def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedTraversal_]] does not contain a given value */
  final def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)

  /** check if the [[IndexedTraversal_]] does not contain a focus */
  final def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[IndexedTraversal_]] contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of an [[IndexedTraversal_]] */
  final def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of an [[IndexedTraversal_]] that satisfies a predicate, if there is any */
  final def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = s => foldRight[Option[(A, I)]](s)(None)((ai, op) => op.fold(if (f(ai)) ai.some else None)(Some[(A, I)]))

  /** synonym for [[preview]] */
  final def first(s: S): Option[(A, I)] = preview(s)

  /** find the last focus and index of an [[IndexedTraversal_]] that satisfies a predicate, if there is any */
  final def last(s: S): Option[(A, I)] = foldMap(s)(ai => Last(ai.some)).runLast

  /** the minimum of all foci of an [[IndexedTraversal_]], if there is any */
  final def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of an [[IndexedTraversal_]], if there is any */
  final def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of an [[IndexedTraversal_]] into an [[Array]] */
  final def toArray(s: S)(implicit ev0: ClassTag[A]): Array[A] = toList(s).toArray

  /** collect all the foci of an [[IndexedTraversal_]] into a [[List]] */
  final def toList(s: S): List[A] = foldMap(s) { case (a, _) => List(a) }

  /** view the focus and the index of an [[IndexedTraversal_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, List[(A, I)]] = ev.inspect(viewAll)

  /** synonym to [[asTraversal]] */
  final def unIndex: Traversal_[S, T, A, B] = asTraversal

  /** remap the index, resulting in a change of type to the full structure */
  final def reindex[J](f: I => J): IndexedTraversal_[J, S, T, A, B] = new IndexedTraversal_[J, S, T, A, B] {
    override private[proptics] def apply[P[_, _]](indexed: Indexed[P, J, A, B])(implicit ev: Wander[P]): P[S, T] =
      self(indexed.reindex[I](f)(ev))
  }

  /** transform an [[IndexedTraversal_]] to a [[Traversal_]] */
  final def asTraversal: Traversal_[S, T, A, B] = Traversal_(new Rank2TypeTraversalLike[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] =
      self(Indexed(ev.dimap[A, B, (A, I), B](pab)(_._1)(identity)))
  })

  /** transform an [[IndexedLens_]] to an [[IndexedFold_]] */
  final def asIndexedFold: IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget))
  }

  /** compose an [[IndexedTraversal_]] with an [[Iso_]] */
  final def compose[C, D](other: Iso_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with an [[AnIso_]] */
  final def compose[C, D](other: AnIso_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with a [[Lens_]] */
  final def compose[C, D](other: Lens_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with an [[ALens_]] */
  final def compose[C, D](other: ALens_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with a [[Prism_]] */
  final def compose[C, D](other: Prism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with an [[APrism_]] */
  final def compose[C, D](other: APrism_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with an [[AffineTraversal_]] */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with an [[AnAffineTraversal_]] */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with a [[Traversal_]] */
  final def compose[C, D](other: Traversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with an [[ATraversal_]] */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        composeWithTraverseFn(f)(other.overF)
    })

  /** compose an [[IndexedTraversal_]] with a [[Setter_]] */
  final def compose[C, D](other: Setter_[A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B] { case (a, i) => other.over(c => indexed.runIndex((c, i)))(a) })
  }

  /** compose an [[IndexedTraversal_]] with a [[Getter_]] */
  final def compose[C, D](other: Getter_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget {
        self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) }
      }
  }

  /** compose an [[IndexedTraversal_]] with a [[Fold_]] */
  final def compose[C, D](other: Fold_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget {
        self.foldMap(_) { case (a, i) => other.foldMap(a)(c => indexed.runIndex.runForget((c, i))) }
      }
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = new IndexedTraversal_[J, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, J, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, J), D] = new Traversing[S, T, (C, J), D] {
        override def apply[F[_]](f: ((C, J)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, _) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[IndexedLens_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, I), D] = new Traversing[S, T, (C, I), D] {
        override def apply[F[_]](f: ((C, I)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, i) => other.overF { case (otherA, _) => f((otherA, i)) }(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = composeWithRightIndex(other.asIndexedLens)

  /** compose an [[IndexedTraversal_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = composeWithLeftIndex(other.asIndexedLens)

  /** compose an [[IndexedTraversal_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = new IndexedTraversal_[J, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, J, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, J), D] = new Traversing[S, T, (C, J), D] {
        override def apply[F[_]](f: ((C, J)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, _) => other.overF(f)(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedTraversal_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def composeWithLeftIndex[_, C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = new IndexedTraversal_[I, S, T, C, D] {
    override def apply[P[_, _]](indexed: Indexed[P, I, C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (C, I), D] = new Traversing[S, T, (C, I), D] {
        override def apply[F[_]](f: ((C, I)) => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.overF { case (a, i) => other.overF { case (otherA, _) => f((otherA, i)) }(a) }(s)
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def <<*[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = new IndexedSetter_[J, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, J, C, D]): S => T =
      self(Indexed[* => *, I, A, B](other(indexed) compose Tuple2._1))
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedSetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedSetter_[J, A, B, C, D]): IndexedSetter_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[IndexedSetter_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed[* => *, I, A, B] { case (a, i) =>
        other.over { case (c, _) => indexed.runIndex((c, i)) }(a)
      })
  }

  /** compose an [[IndexedTraversal_]] with an [[IndexedSetter_]], while preserving self indices */
  final def <<*[C, D](other: IndexedSetter_[_, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] =
    composeWithRightIndex(other.asIndexedFold)

  /** compose an [[IndexedTraversal_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedTraversal_]] with an [[IndexedGetter_]], while preserving self indices */
  final def composeWithLeftIndex[J, C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] =
    composeWithLeftIndex(other.asIndexedFold)

  /** compose an [[IndexedTraversal_]] with an [[IndexedGetter_]], while preserving self indices */
  final def <<*[C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] = {
      val runForget = other(indexed).runForget

      Forget(self.foldMap(_)(runForget compose Tuple2._1))
    }
  }

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) =>
        other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) }
      })
  }

  /** compose [[IndexedTraversal_]] with an [[IndexedFold_]], while preserving self indices */
  final def <<*[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  private def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldRight[Option[A]](s)(None)((pair, op) => f(pair._1, op.getOrElse(pair._1)).some)

  private def composeWithTraverseFn[F[_]: Applicative, C, D](f: ((C, I)) => F[D])(g: (C => F[D]) => A => F[B]): S => F[T] =
    self.overF { case (a, i) => g(c => f((c, i)))(a) }
}

object IndexedTraversal_ {
  /** create a polymorphic [[IndexedTraversal_]] from Rank2TypeIndexedTraversalLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedTraversalLike[I, S, T, A, B]): IndexedTraversal_[I, S, T, A, B] = new IndexedTraversal_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = f(indexed)
  }

  /** create a polymorphic [[IndexedTraversal_]] from a getter/setter pair */
  final def apply[I, S, T, A, B](get: S => (A, I))(_set: S => B => T): IndexedTraversal_[I, S, T, A, B] = new IndexedTraversal_[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing: Traversing[S, T, (A, I), B] = new Traversing[S, T, (A, I), B] {
        override def apply[F[_]](f: ((A, I)) => F[B])(s: S)(implicit ev: Applicative[F]): F[T] =
          ev.map(f(get(s)))(_set(s))
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  /** create a polymorphic [[IndexedTraversal_]] from a combined getter/setter */
  final def traversal[I, S, T, A, B](to: S => ((A, I), B => T)): IndexedTraversal_[I, S, T, A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(indexed.runIndex)
    })

  /** create a polymorphic [[IndexedTraversal_]] from a [[TraverseWithIndex]] */
  final def fromTraverseWithIndex[G[_], I, A, B](implicit ev0: TraverseWithIndex[G, I]): IndexedTraversal_[I, G[A], G[B], A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, G[A], G[B], A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev1: Wander[P]): P[G[A], G[B]] = {
        val traversing = new Traversing[G[A], G[B], (A, I), B] {
          override def apply[F[_]](f: ((A, I)) => F[B])(s: G[A])(implicit ev2: Applicative[F]): F[G[B]] =
            ev0.traverseWithIndex[F, A, B]((a, i) => f((a, i)))(s)
        }

        ev1.wander(traversing)(indexed.runIndex)
      }
    })

  /** create a polymorphic [[IndexedTraversal_]] from a Traverse that has an index ot type Int */
  final def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): IndexedTraversal_[Int, G[A], G[B], A, B] =
    Traversal_.fromTraverse[G, A, B].asIndexableTraversal

  /** create a polymorphic [[IndexedTraversal_]] from a rank 2 type traversal function */
  final def wander[I, S, T, A, B](lensLikeWithIndex: LensLikeWithIndex[I, S, T, A, B]): IndexedTraversal_[I, S, T, A, B] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev0: Wander[P]): P[S, T] = {
        def traversing: Traversing[S, T, (A, I), B] = new Traversing[S, T, (A, I), B] {
          override def apply[F[_]](f: ((A, I)) => F[B])(s: S)(implicit ev1: Applicative[F]): F[T] = lensLikeWithIndex[F](f)(ev1)(s)
        }

        ev0.wander(traversing)(indexed.runIndex)
      }
    })
}

object IndexedTraversal {
  /** create a monomorphic [[IndexedTraversal]] from a getter/setter pair */
  final def apply[I, S, A](get: S => (A, I))(set: S => A => S): IndexedTraversal[I, S, A] = IndexedTraversal_(get)(set)

  /** create a monomorphic [[IndexedTraversal]] from a combined getter/setter. synonym to apply */
  final def traversal[I, S, A](to: S => ((A, I), A => S)): IndexedTraversal[I, S, A] = IndexedTraversal_.traversal(to)

  /** create a monomorphic [[IndexedTraversal_]] from a [[Traverse]] */
  final def fromTraverseWithIndex[F[_], I, A](implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal_.fromTraverseWithIndex[F, I, A, A]

  /** create a monomorphic [[IndexedTraversal_]] from a Traverse that has an index ot type Int */
  final def fromTraverse[F[_], A](implicit ev0: Traverse[F]): IndexedTraversal[Int, F[A], A] =
    IndexedTraversal_.fromTraverse[F, A, A]

  /** create a monomorphic [[IndexedTraversal]] from a rank 2 type traversal function */
  final def wander[I, S, A](lensLikeWithIndex: LensLikeWithIndex[I, S, S, A, A]): IndexedTraversal[I, S, A] =
    IndexedTraversal_.wander[I, S, S, A, A](lensLikeWithIndex)

  /** create a monomorphic [[IndexedTraversal_]] that narrows the focus to a single element */
  final def elementAt[F[_], A](i: Int)(implicit ev0: TraverseWithIndex[F, Int]): Traversal[F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, Int, A].elementAt(i)

  /** create a monomorphic [[IndexedTraversal_]] that takes the longest prefix of elements of a Traverse that satisfy a predicate */
  final def takeWhile[F[_], I, A](predicate: A => Boolean)(implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, I, A].takeWhile(predicate)

  /** create a monomorphic [[IndexedTraversal_]] that takes the longest prefix of elements of a Traverse that satisfy a predicate */
  final def takeWhileWithIndex[F[_], I, A](predicate: ((A, I)) => Boolean)(implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, I, A].takeWhileWithIndex(predicate)

  /** create a monomorphic [[IndexedTraversal_]] that drop longest prefix of elements of a Traverse that satisfy a predicate */
  final def dropWhile[F[_], I, A](predicate: A => Boolean)(implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, I, A].dropWhile(predicate)

  /** create a monomorphic [[IndexedTraversal_]] that drop longest prefix of elements of a Traverse that satisfy a predicate */
  final def dropWhileWithIndex[F[_], I, A](predicate: ((A, I)) => Boolean)(implicit ev0: TraverseWithIndex[F, I]): IndexedTraversal[I, F[A], A] =
    IndexedTraversal.fromTraverseWithIndex[F, I, A].dropWhileWithIndex(predicate)
}
