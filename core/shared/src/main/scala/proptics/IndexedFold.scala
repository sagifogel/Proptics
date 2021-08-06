package proptics

import scala.Function.const
import scala.reflect.ClassTag

import cats.data.State
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Eq, Foldable, Id, Monoid, Order}
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}
import spire.std.boolean._

import proptics.data._
import proptics.indices.FoldableWithIndex
import proptics.internal.{Forget, Indexed}
import proptics.rank2types.Rank2TypeIndexedFoldLike
import proptics.syntax.function._
import proptics.syntax.tuple._

/** A [[IndexedFold_]] is a generalization of something Foldable. It describes how to retrieve multiple values and thier indices.
  *
  * A [[IndexedFold_]] is similar to a [[IndexedTraversal_]], but it cannot modify its foci.
  *
  * An [[IndexedFold_]] is an indexed optic with fixed type [[proptics.internal.Forget]] [[cats.arrow.Profunctor]]
  *
  * @tparam I the index of an [[IndexedFold_]]
  * @tparam S the source of an [[IndexedFold_]]
  * @tparam T the modified source of an [[IndexedFold_]]
  * @tparam A the foci of an [[IndexedFold_]]
  * @tparam B the modified foci of an [[IndexedFold_]]
  */
abstract class IndexedFold_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]

  /** synonym to [[fold]] */
  final def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  /** collect all the foci and indices of an [[IndexedFold_]] into aList */
  final def viewAll(s: S): List[(A, I)] = foldMap(s)(List(_))

  /** view the first focus and index of an [[IndexedFold_]], if there is any */
  final def preview(s: S): Option[(A, I)] = foldMap(s)(ai => First(ai.some)).runFirst

  /** map each focus of an [[IndexedFold_]] to a [[cats.Monoid]], and combine the results */
  final def foldMap[R: Monoid](s: S)(f: ((A, I)) => R): R = self[R](Indexed(Forget(f))).runForget(s)

  /** fold the foci of a [[IndexedFold_]] using a [[cats.Monoid]] */
  final def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(_._1)

  /** fold the foci of an [[IndexedFold_]] using a binary operator, going right to left */
  final def foldRight[R](s: S)(r: R)(f: ((A, I), R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of an [[IndexedFold_]] using a binary operator, going left to right */
  final def foldLeft[R](s: S)(r: R)(f: (R, (A, I)) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** the sum of all foci of an [[IndexedFold_]] */
  final def sum(s: S)(implicit ev: AdditiveMonoid[A]): A =
    foldMap(s)(Additive[A] _ compose Tuple2._1).runAdditive

  /** the product of all foci of an [[IndexedFold_]] */
  final def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A =
    foldMap(s)(Multiplicative[A] _ compose Tuple2._1).runMultiplicative

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedFold_]] */
  final def forall(f: ((A, I)) => Boolean): S => Boolean = s => forall(s)(f)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def forall[R: Heyting](s: S)(f: ((A, I)) => R): R = foldMap(s)(Conj[R] _ compose f).runConj

  /** return the result of a conjunction of all foci of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(_._1)

  /** return the result of a disjunction of all foci of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def or(s: S)(implicit ev: Heyting[A]): A = any[Id, A](s)(_._1)

  /** test whether a predicate holds for any focus and index of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def any[F[_], R: Heyting](s: S)(f: ((A, I)) => R): R = foldMap(s)(Disj[R] _ compose f).runDisj

  /** test whether a predicate holds for any focus and index of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def exists(f: ((A, I)) => Boolean): S => Boolean = s => any[Disj, Boolean](s)(f)

  /** test whether a predicate does not hold for any focus and index of an [[IndexedFold_]] */
  final def notExists(f: ((A, I)) => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a focus at specific index of an [[IndexedFold_]] contains a given value */
  final def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedFold_]] does not contain a given value */
  final def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)

  /** check if the [[IndexedFold_]] does not contain a focus */
  final def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[IndexedFold_]] contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of an [[IndexedFold_]] */
  final def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus and index of an [[IndexedFold_]] that satisfies a predicate, if there is any */
  final def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = s => foldRight[Option[(A, I)]](s)(None)((ia, op) => op.fold(if (f(ia)) ia.some else None)(Some[(A, I)]))

  /** synonym for [[preview]] */
  final def first(s: S): Option[(A, I)] = preview(s)

  /** find the last focus and index of an [[IndexedFold_]] that satisfies a predicate, if there is any */
  final def last(s: S): Option[(A, I)] = foldMap(s)(ai => Last(ai.some)).runLast

  /** the minimum of all foci of an [[IndexedFold_]], if there is any */
  final def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of an `R`, if there is any */
  final def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of an [[IndexedFold_]] into an Array */
  final def toArray(s: S)(implicit ev0: ClassTag[A]): Array[A] = toList(s).toArray

  /** collect all the foci of an [[IndexedFold_]] into aList */
  final def toList(s: S): List[A] = foldMap(s) { case (a, _) => List(a) }

  /** view the focus and the index of an [[IndexedFold_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, List[(A, I)]] = ev.inspect(viewAll)

  /** synonym to [[asFold]] */
  final def unIndex: Fold_[S, T, A, B] = asFold

  /** remap the index, resulting in a change of type to the full structure */
  final def reindex[J](f: I => J): IndexedFold_[J, S, T, A, B] = new IndexedFold_[J, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, A, B]): Forget[R, S, T] =
      self(indexed.reindex[I](f)(Forget.profunctorForget[R]))
  }

  /** transform an [[IndexedFold_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose Tuple2._1))
  }

  /** compose this [[IndexedFold_]] with an [[Iso_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Iso_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with an [[Iso_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Iso_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[AnIso_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: AnIso_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with an [[AnIso_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: AnIso_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[Lens_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Lens_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with an [[Lens_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Lens_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[ALens_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: ALens_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with an [[ALens_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: ALens_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[Prism_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Prism_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[Prism_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Prism_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .map(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) })
          .getOrElse(Monoid[R].empty)
      }
  }

  /** compose this [[IndexedFold_]] with an [[APrism_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: APrism_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[APrism_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: APrism_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .map(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) })
          .getOrElse(Monoid[R].empty)
      }
  }

  /** compose this [[IndexedFold_]] with an [[AffineTraversal_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[AffineTraversal_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .map(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) })
          .getOrElse(Monoid[R].empty)
      }
  }

  /** compose this [[IndexedFold_]] with an [[AffineTraversal_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[AffineTraversal_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        other
          .preview(c)
          .map(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) })
          .getOrElse(Monoid[R].empty)
      }
  }

  /** compose this [[IndexedFold_]] with an [[Traversal_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Traversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[Traversal_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Traversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) }))
  }

  /** compose this [[IndexedFold_]] with an [[ATraversal_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with an [[ATraversal_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: ATraversal_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) }))
  }

  /** compose this [[IndexedFold_]] with a [[Getter_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Getter_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose this [[IndexedFold_]] with a [[Getter_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Getter_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c)) { case (a, i) => indexed.runIndex.runForget((a, i)) })
  }

  /** compose this [[IndexedFold_]] with a function lifted to a [[Getter_]], having this [[IndexedFold_]] applied last */
  final def to[C, D](f: A => C): IndexedFold_[I, S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose this [[IndexedFold_]] with a [[Fold_]], having this [[IndexedFold_]] applied last */
  final def compose[C, D](other: Fold_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose this [[IndexedFold_]] with a [[Fold_]], having this [[IndexedFold_]] applied first */
  final def andThen[C, D](other: Fold_[C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((a, i)) }))
  }

  /** compose this [[IndexedFold_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._1))
  }

  /** compose [[IndexedFold_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedLens_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[IndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._1))
  }

  /** compose [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving self indices */
  final def <<*[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) } })
  }

  /** compose this [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving self indices */
  final def <<*[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => indexed.runIndex.runForget(other.view(a)) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedGetter_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose this [[IndexedFold_]] with an [[IndexedGetter_]], while preserving self indices */
  final def <<*[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def composeWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  final def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose this [[IndexedFold_]] with an [[IndexedFold_]], while preserving self indices */
  final def composeWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) } })
  }

  /** compose this [[IndexedFold_]] with an [[IndexedFold_]], while preserving self indices */
  final def <<*[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose this [[IndexedFold_]] with a function lifted to an [[IndexedGetter_]] */
  final def toWithIndex[C, D](f: A => (C, I)): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(IndexedGetter_[I, A, B, C, D](f))

  private def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldRight[Option[A]](s)(None)((pair, op) => f(pair._1, op.getOrElse(pair._1)).some)
}

object IndexedFold_ {
  /** create a polymorphic [[IndexedFold_]] from Rank2TypeIndexedFoldLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedFoldLike[I, S, T, A, B])(implicit ev: DummyImplicit): IndexedFold_[I, S, T, A, B] =
    new IndexedFold_[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] = f(indexed)
    }

  /** create a polymorphic [[IndexedFold_]] from a getter function */
  final def apply[I, S, T, A, B](get: S => (A, I)): IndexedFold_[I, S, T, A, B] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] =
        Forget(indexed.runIndex.runForget compose get)
    })

  /** create a polymorphic [[IndexedFold_]] from Foldable that has an index ot type Int */
  final def fromFoldable[F[_], A, B](implicit ev0: Foldable[F]): IndexedFold_[Int, F[A], F[B], A, B] =
    Fold_.fromFoldable[F, A, F[B], B].asIndexableFold

  /** create a polymorphic [[IndexedFold_]] from [[proptics.indices.FoldableWithIndex]] */
  final def fromFoldableWithIndex[F[_], I, A, B](implicit ev: FoldableWithIndex[F, I]): IndexedFold_[I, F[A], F[B], A, B] = new IndexedFold_[I, F[A], F[B], A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, F[A], F[B]] =
      Forget(fa => ev.foldMapWithIndex[A, R]((a, i) => indexed.runIndex.runForget((a, i)))(fa))
  }

  /** implicit conversion from [[IndexedLens_]] to [[IndexedFold_]] */
  implicit def indexedLensToIndexedFold[I, S, T, A, B](indexedLens: IndexedLens_[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = indexedLens.asIndexedFold

  /** implicit conversion from [[AnIndexedLens_]] to [[IndexedFold_]] */
  implicit def anIndexedLensToIndexedFold[I, S, T, A, B](anIndexedLens: AnIndexedLens_[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = anIndexedLens.asIndexedFold

  /** implicit conversion from [[Traversal_]] to [[IndexedFold_]] */
  implicit def indexedTraversalToIndexedFold[I, S, T, A, B](indexedTraversal: IndexedTraversal_[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = indexedTraversal.asIndexedFold

  /** implicit conversion from [[IndexedGetter_]] to [[IndexedFold_]] */
  implicit def indexedGetterToIndexedFold[I, S, T, A, B](indexedGetter: IndexedGetter_[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = indexedGetter.asIndexedFold
}

object IndexedFold {
  /** create a monomorphic [[IndexedFold]] from a getter function */
  final def apply[I, S, A](get: S => (A, I)): IndexedFold[I, S, A] = IndexedFold_(get)

  /** create a monomorphic [[IndexedFold]] using a predicate to filter out elements of future optics composed with this [[IndexedFold_]] */
  final def filtered[I, A](predicate: ((A, I)) => Boolean): IndexedFold_[I, (A, I), (A, I), A, A] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, (A, I), (A, I), A, A] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, A])(implicit ev: Monoid[R]): Forget[R, (A, I), (A, I)] =
        Forget { p =>
          if (predicate(p)) indexed.runIndex.runForget((p._1, p._2))
          else ev.empty
        }
    })

  /** create a monomorphic [[IndexedFold]] from [[proptics.indices.FoldableWithIndex]] */
  final def fromFoldableWithIndex[F[_], I, A](implicit ev: FoldableWithIndex[F, I]): IndexedFold[I, F[A], A] =
    IndexedFold_.fromFoldableWithIndex[F, I, A, A]

  /** create a monomorphic [[IndexedFold]] from Foldable */
  final def fromFoldable[F[_], A](implicit ev: Foldable[F]): IndexedFold[Int, F[A], A] = IndexedFold_.fromFoldable[F, A, A]

  /** check to see if an [[IndexedFold]] matches one or more entries */
  final def has[I, S, A](indexedFold: IndexedFold[I, S, A]): S => Boolean = indexedFold.nonEmpty
}
