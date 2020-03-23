package proptics

import cats.arrow.Arrow
import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.arrow._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Eq, Foldable, Id, Monoid, Order}
import proptics.internal.Forget
import proptics.instances.BooleanInstances._
import proptics.newtype._
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
 * A [[AGetter]] is a [[Fold]] which has the same return type as the type of the target of the fold.
 *
 * @tparam S the source of an [[AGetter]]
 * @tparam T the modified source of an [[AGetter]]
 * @tparam A the target of an [[AGetter]]
 * @tparam B the modified target of an [[AGetter]]
 */
abstract class AGetter[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T]

  def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  def viewAll(s: S): List[A] = foldMap(s)(List(_))

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  def foldr(s: S)(a: A)(implicit ev: Monoid[A]): A = foldMap(s)(a => Endo(ev.combine(a, _))).runEndo(a)

  def foldl(s: S)(a: A)(implicit ev: Monoid[A]): A = foldMap(s)(a => Dual(Endo(ev.combine(a, _)))).runDual.runEndo(a)

  def sum(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Additive[A], A](identity)(s)

  def product(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Multiplicative[A], A](identity)(s)

  def all(f: A => Boolean): S => Boolean = allOf(_)(f)

  def and(s: S)(implicit ev: Heyting[A]): A = allOf(s)(identity)

  def or(s: S)(implicit ev: Heyting[A]): A = anyOf[Id, A](s)(identity)

  def exists(f: A => Boolean): S => Boolean = anyOf[Disj, Boolean](_)(f)

  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  def length(s: S)(implicit ev: Monoid[Int]): Int = foldMap(s)(const(1))

  def has(s: S)(implicit ev: Heyting[A]): A = hasOrHasnt(s)(ev.one)

  def hasNot(s: S)(implicit ev: Heyting[A]): A = hasOrHasnt(s)(ev.zero)

  def find(f: A => Boolean): S => Option[A] = foldr[Option[A]](_)(None)(a => _.fold(if (f(a)) a.some else None)(Some[A]))

  def first(s: S): Option[A] = preview(s)

  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](_.some)(s)

  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  def toList(s: S)(implicit ev: Monoid[A]): List[A] = viewAll(s)

  def use[M[_]](implicit ev0: MonadState[M, S], ev1: Monoid[A]): M[A] = ev0.inspect(view)

  def asGetter(implicit ev: Monoid[A]): Getter[S, T, A, B] = Getter(self.view)

  def zip[C: Monoid, D](that: AGetter[S, T, C, D])(implicit ev0: Arrow[* => *], ev1: Monoid[A]): Getter[S, T, (A, C), (B, D)] =
    Getter(self.view _ &&& that.view)

  protected def foldMap[R](s: S)(f: A => R)(implicit ev: Monoid[R]): R

  private[proptics] def hasOrHasnt(s: S)(r: A)(implicit ev: Heyting[A]): A = foldMap(s)(const(Disj(r))).runDisj

  private[proptics] def foldMapNewtype[F, R](f: A => R)(s: S)(implicit ev0: Monoid[F], ev1: Newtype.Aux[F, R]): R =
    ev1.unwrap(foldMap(s)(ev1.wrap _ compose f))

  private[proptics] def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
    foldr[Option[A]](s)(None)(a => op => f(a, op.getOrElse(a)).some)

  private def foldr[R](s: S)(r: R)(f: A => R => R): R = foldMap(s)(Endo[* => *, R] _ compose f).runEndo(r)

  private def allOf[R](s: S)(f: A => R)(implicit ev: Heyting[R]): R = foldMapNewtype[Conj[R], R](f)(s)

  private def anyOf[F[_], R](s: S)(f: A => R)(implicit ev0: Monoid[Disj[R]]): R = foldMapNewtype[Disj[R], R](f)(s)
}

object AGetter {
  private[AGetter] def apply[S, T, A, B](aGetter: Forget[A, A, B] => Forget[A, S, T]): AGetter[S, T, A, B] = new AGetter[S, T, A, B] {
    override def apply(forget: Forget[A, A, B]): Forget[A, S, T] = aGetter(forget)

    override protected def foldMap[R](s: S)(f: A => R)(implicit ev: Monoid[R]): R = {
      val forget = aGetter(Forget[A, A, B](identity))

      f(forget.runForget(s))
    }
  }

  def apply[R, S, T, A, B](f: S => A)(implicit ev: DummyImplicit): AGetter[S, T, A, B] =
    AGetter((forget: Forget[A, A, B]) => Forget[A, S, T](forget.runForget compose f))

  def fromFoldable[F[_], A: Monoid, B, T](implicit ev0: Foldable[F]): AGetter[F[A], B, A, T] = new AGetter[F[A], B, A, T] {
    override private[proptics] def apply(forget: Forget[A, A, T]): Forget[A, F[A], B] =
      Forget[A, F[A], B](ev0.foldMap(_)(forget.runForget))

    override protected def foldMap[R](s: F[A])(f: A => R)(implicit ev1: Monoid[R]): R = ev0.foldMap(s)(f)
  }
}

object AGetter_ {
  def apply[S, A](f: S => A): AGetter_[S, A] = AGetter(f)

  def fromFoldable[F[_] : Foldable, A: Monoid, T]: AGetter[F[A], A, A, T] = AGetter.fromFoldable
}