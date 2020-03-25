package proptics

import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.monoid._
import cats.syntax.option._
import cats.{Eq, Foldable, Id, Monoid, Order}
import proptics.instances.BooleanInstances._
import proptics.internal.Forget
import proptics.newtype.First._
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

  def view(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  def viewAll(s: S)(implicit ev: Monoid[A]): List[A] = foldMap(s)(List(_))

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def foldMap[R: Monoid](s: S)(f: A => R): R = self(Forget(f)).runForget(s)

  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  def foldr[R](s: S)(r: R)(f: A => R => R): R = foldMap(s)(Endo[* => *, R] _ compose f).runEndo(r)

  def foldl[R](s: S)(r: R)(f: R => A => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip).runDual.runEndo(r)

  def sum(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Additive[A], A](identity)(s)

  def product(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Multiplicative[A], A](identity)(s)

  def all(f: A => Boolean): S => Boolean = allOf(_)(f)

  def allOf[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](f)(s)

  def and(s: S)(implicit ev: Heyting[A]): A = allOf(s)(identity)

  def or(s: S)(implicit ev: Heyting[A]): A = anyOf[Id, A](s)(identity)

  def exists(f: A => Boolean): S => Boolean = anyOf[Disj, Boolean](_)(f)

  def anyOf[F[_], R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Disj[R], R](f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def length(s: S): Int = foldMap(s)(const(1))

  def has[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

  def hasNot[R](s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)

  def find(f: A => Boolean): S => Option[A] =
    foldr[Option[A]](_)(None)(a => _.fold(if (f(a)) a.some else None)(Some[A]))

  def first(s: S): Option[A] = preview(s)

  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](_.some)(s)

  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  def toArray[AA >: A](s: S)(implicit ev0: ClassTag[AA], ev1: Monoid[A]): Array[AA] = toList(s).toArray

  def toList(s: S)(implicit ev: Monoid[A]): List[A] = viewAll(s)

  def use[M[_]](implicit ev0: MonadState[M, S], ev1: Monoid[A]): M[List[A]] = ev0.inspect(viewAll)

  private[proptics] def hasOrHasnt[R: Heyting](s: S)(r: R): R = foldMap(s)(const(Disj(r))).runDisj

  private[proptics] def foldMapNewtype[F: Monoid, R](f: A => R)(s: S)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private[proptics] def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
    foldr[Option[A]](s)(None)(a => op => f(a, op.getOrElse(a)).some)
}

object Fold {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeFoldLike[S, T, A, B]): Fold[S, T, A, B] = new Fold[S, T, A, B] { self =>
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = f(forget)
  }

  def apply[S, T, A, B](get: S => A)(implicit ev: DummyImplicit): Fold[S, T, A, B] =
    Fold(fromGetRank2TypeFoldLike[S, T, A, B](get))

  def filtered[P[_, _], A](predicate: A => Boolean)(implicit ev: Choice[P]): Optic_[P, A, A] = {
    Optic_[P, A, A](pab => ev.dimap[Either[A, A], Either[A, A], A, A](ev.right(pab))
      (x => if (predicate(x)) x.asRight[A] else x.asLeft[A])(_.fold(identity, identity)))
  }

  def replicate[A, B, T](i: Int): Fold[A, B, A, T] = Fold(replicateRank2TypeFoldLike[A, B, T](i))

  def fromFoldable[F[_], A, B, T](implicit ev0: Foldable[F]): Fold[F[A], B, A, T] = new Fold[F[A], B, A, T] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, T]): Forget[R, F[A], B] =
      Forget[R, F[A], B](ev0.foldMap(_)(forget.runForget))

    override def foldMap[R](s: F[A])(f: A => R)(implicit ev: Monoid[R]): R = ev0.foldMap(s)(f)
  }

  def unfold[S, T, A, B](f: S => Option[(A, S)]): Fold[S, T, A, B] = Fold(unfoldRank2TypeFoldLike[S, T, A, B](f))

  private[proptics] def fromGetRank2TypeFoldLike[S, T, A, B](get: S => A): Rank2TypeFoldLike[S, T, A, B] = new Rank2TypeFoldLike[S, T, A, B] {
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = liftForget[R, S, T, A, B](get)(forget)
  }

  private[proptics] def replicateRank2TypeFoldLike[A, B, T](i: Int): Rank2TypeFoldLike[A, B, A, T] = new Rank2TypeFoldLike[A, B, A, T] {
    override def apply[R: Monoid](forget: Forget[R, A, T]): Forget[R, A, B] = {
      def go[RR](i: Int, r: RR)(implicit ev: Monoid[RR]): RR = (i, r) match {
        case (0, _) => ev.empty
        case (n, x) => x |+| go(n - 1, x)
      }

      Forget[R, A, B](a => go[R](i, forget.runForget(a)))
    }
  }

  private[proptics] def unfoldRank2TypeFoldLike[S, T, A, B](f: S => Option[(A, S)]): Rank2TypeFoldLike[S, T, A, B] = new Rank2TypeFoldLike[S, T, A, B] {
    def go[R](s: S, forget: Forget[R, A, B])(implicit ev: Monoid[R]): R =
      f(s).fold(ev.empty) { case (a, sn) => forget.runForget(a) |+| go(sn, forget) }

    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = Forget[R, S, T](s => go(s, forget))
  }

  private[proptics] def liftForget[R, S, T, A, B](f: S => A): Forget[R, A, B] => Forget[R, S, T] =
    forget => Forget(forget.runForget compose f)

}

object Fold_ {
  def apply[S, A](f: S => A): Fold_[S, A] = Fold(f)

  def filtered[P[_, _], A](predicate: A => Boolean)(implicit ev: Choice[P]): Optic_[P, A, A] = Fold.filtered(predicate)

  def replicated[A, T](i: Int): Fold[A, A, A, T] = Fold.replicate(i)

  def fromFoldable[F[_] : Foldable, A, T]: Fold[F[A], A, A, T] = Fold.fromFoldable

  def unfold[S, A](f: S => Option[(A, S)]): Fold_[S, A] = Fold.unfold(f)
}
