package proptics

import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.eq._
import cats.syntax.monoid._
import cats.syntax.option._
import cats.{Eq, Foldable, Id, Monoid, Order}
import proptics.instances.BooleanInstances._
import proptics.internal.{Forget, Indexed}
import proptics.newtype._
import proptics.rank2types.Rank2TypeIndexedFoldLike
import proptics.syntax.FunctionSyntax._
import proptics.syntax.Tuple2Syntax._
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
  * A [[IndexedFold_]] is an indexed optic with fixed type [[Forget]] [[cats.arrow.Profunctor]]
  *
  * @tparam I the index of an [[IndexedFold_]]
  * @tparam S the source of an [[IndexedFold_]]
  * @tparam T the modified source of an [[IndexedFold_]]
  * @tparam A the target of an [[IndexedFold_]]
  * @tparam B the modified target of an [[IndexedFold_]]
  */
abstract class IndexedFold_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]

  def view(s: S)(implicit ev: Monoid[(I, A)]): (I, A) = foldMap(s)(identity)

  def viewAll(s: S)(implicit ev: Monoid[(I, A)]): List[(I, A)] = foldMap(s)(List(_))

  def preview(s: S): Option[(I, A)] = foldMapNewtype[First[(I, A)], Option[(I, A)]](s)(_.some)

  def foldMap[R: Monoid](s: S)(f: ((I, A)) => R): R = self[R](Indexed(Forget(f))).runForget(s)

  def fold(s: S)(implicit ev: Monoid[(I, A)]): (I, A) = foldMap(s)(identity)

  def foldr[R](s: S)(r: R)(f: ((I, A)) => R => R): R = foldMap(s)(Endo[* => *, R] _ compose f).runEndo(r)

  def foldl[R](s: S)(r: R)(f: R => ((I, A)) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip).runDual.runEndo(r)

  def sum(s: S)(implicit ev: Semiring[(I, A)]): (I, A) = foldMapNewtype[Additive[(I, A)], (I, A)](s)(identity)

  def product(s: S)(implicit ev: Semiring[(I, A)]): (I, A) = foldMapNewtype[Multiplicative[(I, A)], (I, A)](s)(identity)

  def all(f: ((I, A)) => Boolean): S => Boolean = s => allOf(s)(f)

  def allOf[R: Heyting](s: S)(f: ((I, A)) => R): R = foldMapNewtype[Conj[R], R](s)(f)

  def and(s: S)(implicit ev: Heyting[(I, A)]): (I, A) = allOf(s)(identity)

  def or(s: S)(implicit ev: Heyting[(I, A)]): (I, A) = anyOf[Id, (I, A)](s)(identity)

  def exists(f: ((I, A)) => Boolean): S => Boolean = s => anyOf[Disj, Boolean](s)(f)

  def anyOf[F[_], R: Heyting](s: S)(f: ((I, A)) => R): R = foldMapNewtype[Disj[R], R](s)(f)

  def contains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = !contains(s)(a)

  def length(s: S): Int = foldMap(s)(const(1))

  def has[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

  def hasNot[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)

  def find(f: ((I, A)) => Boolean): S => Option[(I, A)] = s => foldr[Option[(I, A)]](s)(None)(ia => _.fold(if (f(ia)) ia.some else None)(Some[(I, A)]))

  def first(s: S): Option[(I, A)] = preview(s)

  def last(s: S): Option[(I, A)] = foldMapNewtype[Last[(I, A)], Option[(I, A)]](s)(_.some)

  def minimum(s: S)(implicit ev: Order[(I, A)]): Option[(I, A)] = minMax(s)(ev.min)

  def maximum(s: S)(implicit ev: Order[(I, A)]): Option[(I, A)] = minMax(s)(ev.max)

  def toArray[AA >: (I, A)](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[(I, A)]): Array[AA] = toList(s).toArray

  def toList(s: S)(implicit ev: Monoid[(I, A)]): List[(I, A)] = viewAll(s)

  def use[M[_]](implicit ev0: MonadState[M, S], ev1: Monoid[(I, A)]): M[List[(I, A)]] = ev0.inspect(viewAll)

  def asIndexedGetter_(implicit ev: Monoid[(I, A)]): IndexedGetter_[I, S, T, A, B] = new IndexedGetter_[I, S, T, A, B] {
    override private[proptics] def apply(indexed: Indexed[Forget[(I, A), *, *], I, A, B]): Forget[(I, A), S, T] =
      Forget(indexed.runIndex.runForget compose self.view)
  }

  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._2))
  }

  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = self compose other.asIndexedLens_

  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (_, a) => other.foldMap(a)(indexed.runIndex.runForget) })
  }

  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view compose Tuple2._2))
  }

  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_) { case (_, a) => other.foldMap(a)(indexed.runIndex.runForget)  })
  }

  private def hasOrHasnt[R: Heyting](s: S)(r: R): R = foldMap(s)(const(Disj(r))).runDisj

  private def foldMapNewtype[F: Monoid, R](s: S)(f: ((I, A)) => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: ((I, A), (I, A)) => (I, A))(implicit ev: Order[(I, A)]): Option[(I, A)] =
    foldr[Option[(I, A)]](s)(None)(a => op => f(a, op.getOrElse(a)).some)
}

object IndexedFold_ {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedFoldLike[I, S, T, A, B]): IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] = f(indexed)
  }

  def apply[I, S, T, A, B](f: S => (I, A))(implicit ev: DummyImplicit): IndexedFold_[I, S, T, A, B] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      override def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
        Forget(indexed.runIndex.runForget compose f)
    })

  def replicate[A, B, T](i: Int): IndexedFold_[Int, A, B, A, T] = IndexedFold_(replicateRank2TypeIndexedFoldLike[A, B, T](i))

  def fromFoldable[F[_], I, A, B, T](implicit ev0: Foldable[F]): IndexedFold_[I, F[(I, A)], B, A, T] = new IndexedFold_[I, F[(I, A)], B, A, T] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, T]): Forget[R, F[(I, A)], B] =
      Forget(ev0.foldMap(_)(indexed.runIndex.runForget))
  }

  def unfold[I, S, T, A, B](f: S => Option[((I, A), S)]): IndexedFold_[I, S, T, A, B] =
    IndexedFold_(unfoldRank2TypeIndexedFoldLike[I, S, T, A, B](f))

  private[proptics] def replicateRank2TypeIndexedFoldLike[A, B, T](i: Int): Rank2TypeIndexedFoldLike[Int, A, B, A, T] = new Rank2TypeIndexedFoldLike[Int, A, B, A, T] {
    override def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], Int, A, T]): Forget[R, A, B] = {
      def go[RR](i: Int, r: RR)(implicit ev: Monoid[RR]): RR = (i, r) match {
        case (0, _) => ev.empty
        case (n, x) => x |+| go(n - 1, x)
      }

      Forget(a => go[R](i, indexed.runIndex.runForget((i, a))))
    }
  }

  private[proptics] def unfoldRank2TypeIndexedFoldLike[I, S, T, A, B](f: S => Option[((I, A), S)]): Rank2TypeIndexedFoldLike[I, S, T, A, B] =
    new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      def go[R](s: S, forget: Forget[R, (I, A), B])(implicit ev: Monoid[R]): R =
        f(s).fold(ev.empty) { case (a, sn) => forget.runForget(a) |+| go(sn, forget) }

      override def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
        Forget(go(_, indexed.runIndex))
    }
}

object IndexedFold {
  def apply[I, S, A](f: S => (I, A)): IndexedFold[I, S, A] = IndexedFold_(f)

  def fromFoldable[F[_], I, A, T](implicit ev0: Foldable[F]): IndexedFold_[I, F[(I, A)], A, A, T] =
    IndexedFold_.fromFoldable

  def replicate[A, T](i: Int): IndexedFold_[Int, A, A, A, T] = IndexedFold_.replicate(i)

  def unfold[I, S, A](f: S => Option[((I, A), S)]): IndexedFold[I, S, A] = IndexedFold_.unfold(f)
}
