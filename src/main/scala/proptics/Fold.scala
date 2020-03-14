package proptics

import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.monoid._
import cats.syntax.option._
import cats.{Applicative, Eq, Foldable, Id, Monoid, Order}
import proptics.internal.Forget
import proptics.internal.heyting.HeytingInstances._
import proptics.newtype._
import proptics.profunctor.Choice
import proptics.rank2types.Rank2TypeFoldLike
import proptics.syntax.FunctionSyntax._
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
 * A [[Fold]] is an [[Optic]] with fixed type [[Forget]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[Fold]]
 * @tparam T the modified source of a [[Fold]]
 * @tparam A the target of a [[Fold]]
 * @tparam B the modified target of a [[Fold]]
 */
abstract class Fold[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T]

  def view(s: S)(implicit ev: Monoid[A]): List[A] = foldMap(List(_))(s)

  def viewAll(s: S)(implicit ev: Monoid[A]): A = foldMap(identity)(s)
  
  def foldMap[R: Monoid](f: A => R)(s: S): R = self(Forget(f)).runForget(s)

  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(identity)(s)

  def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(ev.pure)(s)

  def traverse_[F[_], R](f: A => F[R])(s: S)(implicit ev: Applicative[F]): F[Unit] =
    foldr[F[Unit]](a => ev.void(f(a)) *> _)(ev.pure(()))(s)

  def foldr[R](f: A => R => R)(r: R)(s: S): R = foldMap(Endo[* => *, R] _ compose f)(s).runEndo(r)

  def foldl[R](f: R => A => R)(r: R)(s: S): R =
    foldMap(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip)(s).runDual.runEndo(r)

  def sum(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Additive[A], A](identity)(s)

  def product(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Multiplicative[A], A](identity)(s)

  def all(f: A => Boolean)(s: S): Boolean = allOf(f)(s)

  def allOf[R](f: A => R)(s: S)(implicit ev: Monoid[Conj[R]]): R = foldMapNewtype[Conj[R], R](f)(s)

  def and(s: S)(implicit ev: Monoid[Conj[A]]): A = allOf(identity[A])(s)

  def or(s: S)(implicit ev: Monoid[Disj[A]]): A = anyOf[Id, A](identity[A])(s)

  def exists[R](f: A => Boolean)(s: S): Boolean = anyOf[Disj, Boolean](f)(s)

  def anyOf[F[_], R](f: A => R)(s: S)(implicit ev0: Monoid[Disj[R]]): R = foldMapNewtype[Disj[R], R](f)(s)

  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  def length(s: S): Int = foldMap(const(1))(s)

  def has[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

  def hasNot[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)

  def preview(s: S)(implicit ev: Monoid[First[A]]): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def find(f: A => Boolean)(s: S): Option[A] = foldr[Option[A]](a => _.fold(if (f(a)) a.some else None)(Some[A]))(None)(s)

  def first(s: S): Option[A] = preview(s)

  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](_.some)(s)

  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  def toList(s: S)(implicit ev: Monoid[A]): List[A] = view(s)

  def use[M[_]](implicit ev0: MonadState[M, S], ev1: Monoid[A]): M[List[A]] = ev0.inspect(view)

  private[proptics] def hasOrHasnt[R](s: S)(r: R)(implicit ev: Heyting[R]): R = foldMap(const(Disj(r)))(s).runDisj

  private[proptics] def foldMapNewtype[F, R](f: A => R)(s: S)(implicit ev0: Monoid[F], ev1: Newtype.Aux[F, R]): R =
    ev1.unwrap(foldMap(ev1.wrap _ compose f)(s))

  private[proptics] def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
    foldr[Option[A]](a => op => f(a, op.getOrElse(a)).some)(None)(s)
}

object Fold {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeFoldLike[S, T, A, B]): Fold[S, T, A, B] = new Fold[S, T, A, B] { self =>
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = f(forget)
  }

  private[proptics] def liftForget[R, S, T, A, B](f: S => A): Forget[R, A, B] => Forget[R, S, T] =
    forget => Forget(forget.runForget compose f)

  def apply[S, T, A, B](f: S => A)(implicit ev: DummyImplicit): Fold[S, T, A, B] =
    Fold(new Rank2TypeFoldLike[S, T, A, B] {
      override def apply[R: Monoid](pab: Forget[R, A, B]): Forget[R, S, T] = Forget(pab.runForget compose f)
    })

  def filtered[P[_, _], A](predicate: A => Boolean)(implicit ev: Choice[P]): Optic_[P, A, A] = {
    Optic_[P, A, A](pab => ev.dimap[Either[A, A], Either[A, A], A, A](ev.right(pab))
      (x => if (predicate(x)) x.asRight[A] else x.asLeft[A])(_.fold(identity, identity)))
  }

  def replicated[A, B, T](i: Int): Fold[A, B, A, T] = {
    Fold(new Rank2TypeFoldLike[A, B, A, T] {
      override def apply[R: Monoid](pab: Forget[R, A, T]): Forget[R, A, B] = {
        def go[RR](i: Int, r: RR)(implicit ev: Monoid[RR]): RR = (i, r) match {
          case (0, _) => ev.empty
          case (n, x) => x |+| go(n - 1, x)
        }

        Forget[R, A, B](a => go[R](i, pab.runForget(a)))
      }
    })
  }

  def fromFoldable[F[_], A, B, T](implicit ev0: Foldable[F]): Fold[F[A], B, A, T] = new Fold[F[A], B, A, T] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, T]) =
      Forget[R, F[A], B](ev0.foldMap(_)(forget.runForget))

    override def foldMap[R](f: A => R)(s: F[A])(implicit ev: Monoid[R]): R = ev0.foldMap(s)(f)
}

  def unfolded[S, T, A, B](f: S => Option[(A, S)]): Fold[S, T, A, B] = {
    def go[R](s: S, forget: Forget[R, A, B])(implicit ev: Monoid[R]): R =
      f(s).fold(ev.empty) { case (a, sn) => forget.runForget(a) |+| go(sn, forget) }

    Fold(new Rank2TypeFoldLike[S, T, A, B] {
      override def apply[R: Monoid](pab: Forget[R, A, B]): Forget[R, S, T] = Forget[R, S, T](s => go(s, pab))
    })
  }
}

object Fold_ {
  def apply[S, A](f: S => A): Fold_[S, A] = Fold(f)

  def filtered[P[_, _], A](predicate: A => Boolean)(implicit ev: Choice[P]): Optic_[P, A, A] = Fold.filtered(predicate)

  def replicated[A, T](i: Int): Fold[A, A, A, T] = Fold.replicated(i)

  def fromFoldable[F[_] : Foldable, A, T]: Fold[F[A], A, A, T] = Fold.fromFoldable

  def unfolded[S, A](f: S => Option[(A, S)]): Fold_[S, A] = Fold.unfolded(f)
}
