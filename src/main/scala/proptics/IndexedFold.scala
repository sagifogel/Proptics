package proptics

import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Eq, Id, Monoid, Order}
import proptics.instances.BooleanInstances._
import proptics.internal.{Forget, Indexed}
import proptics.newtype._
import proptics.rank2types.Rank2TypeIndexedFoldLike
import proptics.syntax.FunctionSyntax._
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
 * A [[IndexedFold]] is an [[IndexedOptic]] with fixed type [[Forget]] [[cats.arrow.Profunctor]]
 *
 * @tparam I the index of an [[IndexedFold]]
 * @tparam S the source of an [[IndexedFold]]
 * @tparam T the modified source of an [[IndexedFold]]
 * @tparam A the target of an [[IndexedFold]]
 * @tparam B the modified target of an [[IndexedFold]]
 */
abstract class IndexedFold[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]

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

  def contains(s: S)(a: ((I, A)))(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: ((I, A)))(implicit ev: Eq[(I, A)]): Boolean = !contains(s)(a)

  def length(s: S): Int = foldMap(s)(const(1))

  def has[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

  def hasNot[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)

  def find(f: ((I, A)) => Boolean): S => Option[(I, A)] = s =>
    foldr[Option[(I, A)]](s)(None)(ia => _.fold(if (f(ia)) ia.some else None)(Some[(I, A)]))

  def first(s: S): Option[(I, A)] = preview(s)

  def last(s: S): Option[(I, A)] = foldMapNewtype[Last[(I, A)], Option[(I, A)]](s)(_.some)

  def minimum(s: S)(implicit ev: Order[(I, A)]): Option[(I, A)] = minMax(s)(ev.min)

  def maximum(s: S)(implicit ev: Order[(I, A)]): Option[(I, A)] = minMax(s)(ev.max)

  def toArray[AA >: (I, A)](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[(I, A)]): Array[AA] = toList(s).toArray

  def toList(s: S)(implicit ev: Monoid[(I, A)]): List[(I, A)] = viewAll(s)

  def use[M[_]](implicit ev0: MonadState[M, S], ev1: Monoid[(I, A)]): M[List[(I, A)]] = ev0.inspect(viewAll)

  def asAGetter: AGetter[S, T, A, B] = new AGetter[S, T, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T] =
      Forget(s => {
        val indexed: Indexed[Forget[A, *, *], I, A, B] = Indexed(Forget { case (_, a) => a })

        forget.runForget(self[A](indexed).runForget(s))
      })

    override protected def foldMap[R: Monoid](s: S)(f: A => R): R = self.foldMap(s)(ia => f(ia._2))
}

  private def hasOrHasnt[R: Heyting](s: S)(r: R): R = foldMap(s)(const(Disj(r))).runDisj

  private def foldMapNewtype[F: Monoid, R](s: S)(f: ((I, A)) => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def minMax(s: S)(f: ((I, A), (I, A)) => (I, A))(implicit ev: Order[(I, A)]): Option[(I, A)] =
    foldr[Option[(I, A)]](s)(None)(a => op => f(a, op.getOrElse(a)).some)
}

object IndexedFold {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedFoldLike[I, S, T, A, B]): IndexedFold[I, S, T, A, B] = new IndexedFold[I, S, T, A, B] {
    override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] = f(indexed)
  }

  def apply[I, S, T, A, B](f: S => (I, A))(implicit ev: DummyImplicit): IndexedFold[I, S, T, A, B] =
    IndexedFold(new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
        liftForget[R, I, S, T, A, B](f)(indexed)
    })

  private[proptics] def liftForget[R, I, S, T, A, B](f: S => (I, A)): Indexed[Forget[R, *, *], I, A, B] => Forget[R, S, T] =
    indexed => Forget[R, S, T](indexed.runIndex.runForget compose f)
}

object IndexedFold_ {
  def apply[I, S, A](f: S => (I, A)): IndexedFold_[I, S, A] = IndexedFold(f)
}
