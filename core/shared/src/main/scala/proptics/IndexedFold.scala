package proptics

import scala.Function.const
import scala.annotation.tailrec
import scala.reflect.ClassTag

import cats.data.State
import cats.syntax.eq._
import cats.syntax.monoid._
import cats.syntax.option._
import cats.{Eq, Eval, Id, Later, Monoid, Order}
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid, Ring}
import spire.std.boolean._

import proptics.indices.FoldableWithIndex
import proptics.internal.{Forget, Indexed}
import proptics.newtype._
import proptics.rank2types.Rank2TypeIndexedFoldLike
import proptics.syntax.function._
import proptics.syntax.tuple._

/** A [[IndexedFold_]] is an indexed optic with fixed type [[Forget]] [[cats.arrow.Profunctor]]
  *
  * @tparam I the index of an [[IndexedFold_]]
  * @tparam S the source of an [[IndexedFold_]]
  * @tparam T the modified source of an [[IndexedFold_]]
  * @tparam A the foci of an [[IndexedFold_]]
  * @tparam B the modified foci of an [[IndexedFold_]]
  */
abstract class IndexedFold_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]

  /** collect all the foci and indices of an [[IndexedFold_]] into a [[List]] */
  def viewAll(s: S): List[(A, I)] = foldMap(s)(List(_))

  /** view the first focus and index of an [[IndexedFold_]], if there is any */
  def preview(s: S): Option[(A, I)] = foldMapNewtype[First[(A, I)], Option[(A, I)]](s)(_.some)

  /** map each focus of an [[IndexedFold_]] to a Monoid, and combine the results */
  def foldMap[R: Monoid](s: S)(f: ((A, I)) => R): R = self[R](Indexed(Forget(f))).runForget(s)

  /** fold the foci of an [[IndexedFold_]] using a binary operator, going right to left */
  def foldRight[R](s: S)(r: R)(f: ((A, I), R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of an [[IndexedFold_]] using a binary operator, going left to right */
  def foldLeft[R](s: S)(r: R)(f: (R, (A, I)) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** the sum of all foci of an [[IndexedFold_]] */
  def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMapNewtype[Additive[A], A](s)(_._1)

  /** the product of all foci of an [[IndexedFold_]] */
  def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMapNewtype[Multiplicative[A], A](s)(_._1)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedFold_]] */
  def forall(f: ((A, I)) => Boolean): S => Boolean = s => forall(s)(f)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: ((A, I)) => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** return the result of a conjunction of all foci of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(_._1)

  /** return the result of a disjunction of all foci of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def or(s: S)(implicit ev: Heyting[A]): A = any[Id, A](s)(_._1)

  /** test whether a predicate holds for any focus and index of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def any[F[_], R: Heyting](s: S)(f: ((A, I)) => R): R = foldMapNewtype[Disj[R], R](s)(f)

  /** test whether a predicate holds for any focus and index of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def exists(f: ((A, I)) => Boolean): S => Boolean = s => any[Disj, Boolean](s)(f)

  /** test whether a predicate does not hold for any focus and index of an [[IndexedFold_]] */
  def notExists(f: ((A, I)) => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a focus at specific index of an [[IndexedFold_]] contains a given value */
  def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedFold_]] does not contain a given value */
  def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)

  /** check if the [[IndexedFold_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[IndexedFold_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of an [[IndexedFold_]] */
  def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus and index of an [[IndexedFold_]] that satisfies a predicate, if there is any */
  def find(f: ((A, I)) => Boolean): S => Option[A] = s => {
    foldRight[Option[A]](s)(None)((ia, op) => op.fold(if (f(ia)) ia._1.some else None)(Some[A]))
  }

  /** synonym for [[preview]] */
  def first(s: S): Option[(A, I)] = preview(s)

  /** find the last focus and index of an [[IndexedFold_]] that satisfies a predicate, if there is any */
  def last(s: S): Option[(A, I)] = foldMapNewtype[Last[(A, I)], Option[(A, I)]](s)(_.some)

  /** the minimum of all foci of an [[IndexedFold_]], if there is any */
  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of an [[R]], if there is any */
  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of an [[IndexedFold_]] into an [[Array]] */
  def toArray(s: S)(implicit ev0: ClassTag[A]): Array[A] = toList(s).toArray

  /** collect all the foci of an [[IndexedFold_]] into a [[List]] */
  def toList(s: S): List[A] = foldMap(s) { case (a, _) => List(a) }

  /** view the focus and the index of an [[IndexedFold_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, List[(A, I)]] = ev.inspect(viewAll)

  /** synonym to [[asFold]] */
  def unIndex: Fold_[S, T, A, B] = asFold

  /** remap the index, resulting in a change of type to the full structure */
  def reindex[J](f: I => J): IndexedFold_[J, S, T, A, B] = new IndexedFold_[J, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, A, B]): Forget[R, S, T] =
      self(indexed.reindex[I](f)(Forget.profunctorForget[R]))
  }

  /** transform an [[IndexedFold_]] to a [[Fold_]] */
  def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose Tuple2._1))
  }

  /** compose an [[IndexedFold_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose an [[IndexedFold_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose an [[IndexedFold_]] with an [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose an [[IndexedFold_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose an [[IndexedFold_]] with an [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose an [[IndexedFold_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose an [[IndexedFold_]] with an [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose an [[IndexedFold_]] with an [[AffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose an [[IndexedFold_]] with an [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose an [[IndexedFold_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.preview(a).fold(Monoid[R].empty)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose an [[IndexedFold_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a), i)) })
  }

  /** compose an [[IndexedFold_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a)(c => indexed.runIndex.runForget((c, i))) })
  }

  /** compose an [[IndexedFold_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._1))
  }

  /** compose [[IndexedFold_]] with an [[IndexedLens_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedFold_]] with an [[IndexedLens_]], while preserving self indices */
  def composeWithLeftIndex[C, D](other: IndexedLens_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose an [[IndexedFold_]] with an [[IndexedLens_]], while preserving self indices */
  def <<*[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._1))
  }

  /** compose [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: AnIndexedLens_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving self indices */
  def composeWithLeftIndex[C, D](other: AnIndexedLens_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose an [[IndexedFold_]] with an [[AnIndexedLens_]], while preserving self indices */
  def <<*[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedTraversal_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving self indices */
  def composeWithLeftIndex[C, D](other: IndexedTraversal_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) } })
  }

  /** compose an [[IndexedFold_]] with an [[IndexedTraversal_]], while preserving self indices */
  def <<*[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedFold_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => indexed.runIndex.runForget(other.view(a)) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedGetter_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedGetter_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedFold_]] with an [[IndexedGetter_]], while preserving self indices */
  def composeWithLeftIndex[C, D](other: IndexedGetter_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => indexed.runIndex.runForget((other.view(a)._1, i)) })
  }

  /** compose an [[IndexedFold_]] with an [[IndexedGetter_]], while preserving self indices */
  def <<*[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedFold_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  def composeWithRightIndex[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = new IndexedFold_[J, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, _) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  /** compose [[IndexedFold_]] with an [[IndexedFold_]], while preserving the indices of the other optic */
  def *>>[J, C, D](other: IndexedFold_[J, A, B, C, D]): IndexedFold_[J, S, T, C, D] = composeWithRightIndex(other)

  /** compose an [[IndexedFold_]] with an [[IndexedFold_]], while preserving self indices */
  def composeWithLeftIndex[C, D](other: IndexedFold_[_, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (a, i) => other.foldMap(a) { case (c, _) => indexed.runIndex.runForget((c, i)) } })
  }

  /** compose an [[IndexedFold_]] with an [[IndexedFold_]], while preserving self indices */
  def <<*[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(other)

  /** compose an [[IndexedFold_]] with a function lifted to an [[IndexedGetter_]] */
  def to[C, D](f: A => (C, I)): IndexedFold_[I, S, T, C, D] = composeWithLeftIndex(IndexedGetter_[I, A, B, C, D](f))

  private def foldMapNewtype[F: Monoid, R](s: S)(f: ((A, I)) => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
    foldRight[Option[A]](s)(None)((pair, op) => f(pair._1, op.getOrElse(pair._1)).some)
}

object IndexedFold_ {
  /** create a polymorphic [[IndexedFold_]] from Rank2TypeIndexedFoldLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedFoldLike[I, S, T, A, B])(implicit ev: DummyImplicit): IndexedFold_[I, S, T, A, B] =
    new IndexedFold_[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] = f(indexed)
    }

  /** create a polymorphic [[IndexedFold_]] from a getter function */
  def apply[I, S, T, A, B](get: S => (A, I)): IndexedFold_[I, S, T, A, B] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] =
        Forget(indexed.runIndex.runForget compose get)
    })

  /** create a polymorphic [[IndexedFold_]] by replicating the elements of a fold */
  def replicate[I, A, B](i: Int)(implicit ev: Ring[I]): IndexedFold_[I, A, B, A, B] = new IndexedFold_[I, A, B, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, A, B] = {
      @tailrec
      def go(count: Int, i: I, acc: Eval[R], a: A): Eval[R] =
        if (count === 0) acc
        else go(count - 1, ev.plus(i, ev.one), acc.map(_ |+| indexed.runIndex.runForget((a, i))), a)

      Forget(a => go(i, ev.zero, Later(Monoid[R].empty), a).value)
    }
  }

  /** create a polymorphic [[IndexedFold_]] using an unfold function */
  def unfold[I, S, T, A, B](f: S => Option[((A, I), S)]): IndexedFold_[I, S, T, A, B] =
    IndexedFold_(unfoldRank2TypeIndexedFoldLike[I, S, T, A, B](f))

  private[proptics] def unfoldRank2TypeIndexedFoldLike[I, S, T, A, B](f: S => Option[((A, I), S)]): Rank2TypeIndexedFoldLike[I, S, T, A, B] =
    new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      def go[R](s: S, forget: Forget[R, (A, I), B])(implicit ev: Monoid[R]): R =
        f(s).fold(ev.empty) { case (a, sn) => forget.runForget(a) |+| go(sn, forget) }

      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] =
        Forget(go(_, indexed.runIndex))
    }
}

object IndexedFold {
  /** create a monomorphic [[IndexedFold]] from a getter function */
  def apply[I, S, A](get: S => (A, I)): IndexedFold[I, S, A] = IndexedFold_(get)

  /** create a monomorphic [[IndexedFold]] using a predicate to filter out elements of future optics composed with this [[IndexedFold_]] */
  def filtered[I, A](predicate: ((A, I)) => Boolean): IndexedFold_[I, (A, I), (A, I), A, A] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, (A, I), (A, I), A, A] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, A])(implicit ev: Monoid[R]): Forget[R, (A, I), (A, I)] =
        Forget { p =>
          if (predicate(p)) indexed.runIndex.runForget((p._1, p._2))
          else ev.empty
        }
    })

  /** create a monomorphic [[IndexedFold]] by replicating the elements of a fold */
  def replicate[I: Ring, A](i: Int): IndexedFold[I, A, A] = IndexedFold_.replicate(i)

  /** create a monomorphic [[IndexedFold]] from [[FoldableWithIndex]] */
  def fromFoldable[F[_], I, A](implicit ev: FoldableWithIndex[F, I]): IndexedFold[I, F[A], A] = new IndexedFold[I, F[A], A] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, A]): Forget[R, F[A], F[A]] =
      Forget(ev.foldMapWithIndex((a, i) => indexed.runIndex.runForget((a, i))))
  }

  /** create a monomorphic [[IndexedFold]] using an unfold function */
  def unfold[I, S, A](f: S => Option[((A, I), S)]): IndexedFold[I, S, A] = IndexedFold_.unfold(f)
}
