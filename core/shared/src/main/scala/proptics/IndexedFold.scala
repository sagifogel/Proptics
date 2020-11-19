package proptics

import scala.Function.const
import scala.annotation.tailrec
import scala.reflect.ClassTag

import cats.data.State
import cats.syntax.eq._
import cats.syntax.monoid._
import cats.syntax.option._
import cats.{Eq, Eval, Foldable, Id, Later, Monoid, Order}
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid, Ring}
import spire.std.boolean._

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
  def viewAll(s: S): List[(I, A)] = foldMap(s)(List(_))

  /** view the first focus and index of an [[IndexedFold_]], if there is any */
  def preview(s: S): Option[(I, A)] = foldMapNewtype[First[(I, A)], Option[(I, A)]](s)(_.some)

  /** map each focus of an [[IndexedFold_]] to a Monoid, and combine the results */
  def foldMap[R: Monoid](s: S)(f: ((I, A)) => R): R = self[R](Indexed(Forget(f))).runForget(s)

  /** fold the foci of an [[IndexedFold_]] using a binary operator, going right to left */
  def foldr[R](s: S)(r: R)(f: ((I, A), R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of an [[IndexedFold_]] using a binary operator, going left to right */
  def foldl[R](s: S)(r: R)(f: (R, (I, A)) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** the sum of all foci of an [[IndexedFold_]] */
  def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMapNewtype[Additive[A], A](s)(_._2)

  /** the product of all foci of an [[IndexedFold_]] */
  def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMapNewtype[Multiplicative[A], A](s)(_._2)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedFold_]] */
  def forall(f: ((I, A)) => Boolean): S => Boolean = s => forall(s)(f)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: ((I, A)) => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** return the result of a conjunction of all foci of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(_._2)

  /** return the result of a disjunction of all foci of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def or(s: S)(implicit ev: Heyting[A]): A = any[Id, A](s)(_._2)

  /** test whether a predicate holds for any focus and index of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def any[F[_], R: Heyting](s: S)(f: ((I, A)) => R): R = foldMapNewtype[Disj[R], R](s)(f)

  /** test whether a predicate holds for any focus and index of an [[IndexedFold_]], using a [[Heyting]] algebra */
  def exists(f: ((I, A)) => Boolean): S => Boolean = s => any[Disj, Boolean](s)(f)

  /** test whether a predicate does not hold for any focus and index of an [[IndexedFold_]] */
  def notExists(f: ((I, A)) => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a focus at specific index of an [[IndexedFold_]] contains a given value */
  def contains(a: (I, A))(s: S)(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedFold_]] does not contain a given value */
  def notContains(a: (I, A))(s: S)(implicit ev: Eq[(I, A)]): Boolean = !contains(a)(s)

  /** check if the [[IndexedFold_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[IndexedFold_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of an [[IndexedFold_]] */
  def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus and index of an [[IndexedFold_]] that satisfies a predicate, if there is any */
  def find(f: ((I, A)) => Boolean): S => Option[A] = s => foldr[Option[A]](s)(None)((ia, op) => op.fold(if (f(ia)) ia._2.some else None)(Some[A]))

  /** synonym for [[preview]] */
  def first(s: S): Option[(I, A)] = preview(s)

  /** find the last focus and index of an [[IndexedFold_]] that satisfies a predicate, if there is any */
  def last(s: S): Option[(I, A)] = foldMapNewtype[Last[(I, A)], Option[(I, A)]](s)(_.some)

  /** the minimum of all foci of an [[IndexedFold_]], if there is any */
  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of an [[R]], if there is any */
  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of an [[IndexedFold_]] into an [[Array]] */
  def toArray[AA >: (I, A)](s: S)(implicit ev0: ClassTag[AA]): Array[AA] = toList(s).toArray

  /** synonym to [[viewAll]] */
  def toList(s: S): List[(I, A)] = viewAll(s)

  /** view the focus and the index of an [[IndexedFold_]] in the state of a monad */
  def use(implicit ev: State[S, A]): State[S, List[(I, A)]] = ev.inspect(viewAll)

  /** synonym to [[asFold]] */
  def unIndex: Fold_[S, T, A, B] = asFold

  /** remap the index, resulting in a change of type to the full structure */
  def reindex[J](f: ((I, A)) => (J, A)): IndexedFold_[J, S, T, A, B] = new IndexedFold_[J, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], J, A, B]): Forget[R, S, T] =
      self(indexed.reindex[I](f)(Forget.profunctorForget[R]))
  }

  /** transform an [[IndexedFold_]] to a [[Fold_]] */
  def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose Tuple2._2))
  }

  /** compose an [[IndexedFold_]] with an [[IndexedLens_]] */
  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._2))
  }

  /** compose an [[IndexedFold_]] with an [[AnIndexedLens_]] */
  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = self compose other.asIndexedLens

  /** compose an [[IndexedFold_]] with an [[IndexedTraversal_]] */
  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (_, a) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  /** compose an [[IndexedFold_]] with an [[IndexedGetter_]] */
  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._2))
  }

  /** compose an [[IndexedFold_]] with an [[IndexedFold_]] */
  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (_, a) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  private def foldMapNewtype[F: Monoid, R](s: S)(f: ((I, A)) => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
    foldr[Option[A]](s)(None)((pair, op) => f(pair._2, op.getOrElse(pair._2)).some)
}

object IndexedFold_ {
  /** create a polymorphic [[IndexedFold_]] from Rank2TypeIndexedFoldLike encoding */
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedFoldLike[I, S, T, A, B])(implicit ev: DummyImplicit): IndexedFold_[I, S, T, A, B] =
    new IndexedFold_[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] = f(indexed)
    }

  /** create a polymorphic [[IndexedFold_]] from a getter function */
  def apply[I, S, T, A, B](get: S => (I, A)): IndexedFold_[I, S, T, A, B] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] =
        Forget(indexed.runIndex.runForget compose get)
    })

  /** create a polymorphic [[IndexedFold_]] using a predicate to filter out elements of future optics composed with this [[IndexedFold_]] */
  def filtered[I, A](predicate: ((I, A)) => Boolean): IndexedFold_[I, (I, A), (I, A), A, A] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, (I, A), (I, A), A, A] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, A])(implicit ev: Monoid[R]): Forget[R, (I, A), (I, A)] =
        Forget { p =>
          if (predicate(p)) indexed.runIndex.runForget((p._1, p._2))
          else ev.empty
        }
    })

  /** create a polymorphic [[IndexedFold_]] by replicating the elements of a fold */
  def replicate[I, A, B](i: Int)(implicit ev: Ring[I]): IndexedFold_[I, A, B, A, B] = new IndexedFold_[I, A, B, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, A, B] = {
      @tailrec
      def go(count: Int, i: I, acc: Eval[R], a: A): Eval[R] =
        if (count === 0) acc
        else go(count - 1, ev.plus(i, ev.one), acc.map(_ |+| indexed.runIndex.runForget((i, a))), a)

      Forget(a => go(i, ev.zero, Later(Monoid[R].empty), a).value)
    }
  }

  /** create a polymorphic [[IndexedFold_]] from [[Foldable]] */
  def fromFoldable[I, F[_], A, B, T](implicit ev0: Foldable[F]): IndexedFold_[I, F[(I, A)], B, A, T] = new IndexedFold_[I, F[(I, A)], B, A, T] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, T]): Forget[R, F[(I, A)], B] =
      Forget(ev0.foldMap(_)(indexed.runIndex.runForget))
  }

  /** create a polymorphic [[IndexedFold_]] using an unfold function */
  def unfold[I, S, T, A, B](f: S => Option[((I, A), S)]): IndexedFold_[I, S, T, A, B] =
    IndexedFold_(unfoldRank2TypeIndexedFoldLike[I, S, T, A, B](f))

  private[proptics] def unfoldRank2TypeIndexedFoldLike[I, S, T, A, B](f: S => Option[((I, A), S)]): Rank2TypeIndexedFoldLike[I, S, T, A, B] =
    new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      def go[R](s: S, forget: Forget[R, (I, A), B])(implicit ev: Monoid[R]): R =
        f(s).fold(ev.empty) { case (a, sn) => forget.runForget(a) |+| go(sn, forget) }

      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] =
        Forget(go(_, indexed.runIndex))
    }
}

object IndexedFold {
  /** create a monomorphic [[IndexedFold]] from a getter function */
  def apply[I, S, A](get: S => (I, A)): IndexedFold[I, S, A] = IndexedFold_(get)

  /** create a monomorphic [[IndexedFold]] using a predicate to filter out elements of future optics composed with this [[IndexedFold_]] */
  def filtered[I, A](predicate: ((I, A)) => Boolean): IndexedFold[I, (I, A), A] = IndexedFold_.filtered(predicate)

  /** create a monomorphic [[IndexedFold]] by replicating the elements of a fold */
  def replicate[I: Ring, A](i: Int): IndexedFold[I, A, A] = IndexedFold_.replicate(i)

  /** create a monomorphic [[IndexedFold]] from [[Foldable]] */
  def fromFoldable[F[_]: Foldable, I, A]: IndexedFold_[I, F[(I, A)], (I, A), A, A] =
    IndexedFold_.fromFoldable[I, F, A, (I, A), A]

  /** create a monomorphic [[IndexedFold]] using an unfold function */
  def unfold[I, S, A](f: S => Option[((I, A), S)]): IndexedFold[I, S, A] = IndexedFold_.unfold(f)
}
