package proptics

import cats.instances.int._
import cats.instances.list._
import cats.mtl.MonadState
import cats.syntax.eq._
import cats.syntax.monoid._
import cats.instances.function._
import cats.syntax.option._
import cats.{Eq, Foldable, Monoid, Order}
import proptics.instances.boolean._
import proptics.internal.Forget
import proptics.newtype.First._
import proptics.newtype._
import proptics.rank2types.Rank2TypeFoldLike
import proptics.syntax.function._
import spire.algebra.{MultiplicativeMonoid, Semiring}
import spire.algebra.lattice.Heyting

import scala.Function.const
import scala.reflect.ClassTag

/**
  * A [[Fold_]] is an Optic with fixed type [[Forget]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of a [[Fold_]]
  * @tparam T the modified source of a [[Fold_]]
  * @tparam A the foci of a [[Fold_]]
  * @tparam B the modified foci of a [[Fold_]]
  */
abstract class Fold_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T]

  /** synonym to [[fold]] */
  def view(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  /** collect all the foci of a [[Fold_]] into a [[List]] */
  def viewAll(s: S): List[A] = foldMap(s)(List(_))

  /** view the first focus of a [[Fold_]], if there is any  */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  /** map each focus of a [[Fold_]] to a [[Monoid]], and combine the results */
  def foldMap[R: Monoid](s: S)(f: A => R): R = self(Forget(f)).runForget(s)

  /** fold the foci of a [[Fold_]] using a [[Monoid]] */
  def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  /** fold the foci of a [[Fold_]] using a binary operator, going right to left */
  def foldr[R](s: S)(r: R)(f: (A, R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of a [[Fold_]] using a binary operator, going left to right */
  def foldl[R](s: S)(r: R)(f: (R, A) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** the sum of all foci of a [[Fold_]] */
  def sum(s: S)(implicit ev: Semiring[A]): A = foldMapNewtype[Additive[A], A](s)(identity)

  /** the product of all foci of a [[Fold_]] */
  def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMapNewtype[Multiplicative[A], A](s)(identity)

  /** test whether there is no focus or a predicate holds for all foci of a [[Fold_]] */
  def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for all foci of a [[Fold_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** return the result of a conjunction of all foci of a [[Fold_]], using a [[Heyting]] algebra */
  def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(identity)

  /** returns the result of a disjunction of all foci of a [[Fold_]], using a [[Heyting]] algebra */
  def or(s: S)(implicit ev: Heyting[A]): A = any[A](s)(identity)

  /** test whether a predicate holds for any focus of a [[Fold_]], using a [[Heyting]] algebra */
  def any[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Disj[R], R](s)(f)

  /** test whether a predicate holds for any foci of a [[Fold_]] */
  def exists(f: A => Boolean): S => Boolean = any[Boolean](_)(f)

  /** test whether a predicate does not hold for the foci of a [[Fold_]] */
  def notExists(f: A => Boolean): S => Boolean = any[Boolean](_)(f)

  /** test whether a [[Fold_]] contains a specific focus */
  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether a [[Fold_]] does not contain a specific focus */
  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  /** check if the [[Fold_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[Fold_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of a [[Fold_]] */
  def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of a [[Fold_]] that satisfies a predicate, if there is any */
  def find(f: A => Boolean): S => Option[A] =
    foldr[Option[A]](_)(None)((a, op) => op.fold(if (f(a)) a.some else None)(Some[A]))

  /** find the first focus of a [[Fold_]], if there is any. Synonym for preview */
  def first(s: S): Option[A] = preview(s)

  /** find the last focus of a [[Fold_]], if there is any */
  def last(s: S): Option[A] = foldMapNewtype[Last[A], Option[A]](s)(_.some)

  /** the minimum of all foci of a [[Fold_]], if there is any */
  def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of a [[Fold_]], if there is any */
  def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of a [[Fold_]] into an [[Array]] */
  def toArray[AA >: A](s: S)(implicit ev: ClassTag[AA]): Array[AA] = toList(s).toArray

  /** synonym to [[viewAll]] */
  def toList(s: S): List[A] = viewAll(s)

  /** collect all the foci of a [[Fold_]] in the state of a monad */
  def use[M[_]](implicit ev: MonadState[M, S]): M[List[A]] = ev.inspect(viewAll)

  /** compose a [[Fold_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      self(other(forget)(Forget.profunctorForget))
  }

  /** compose a [[Fold_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asIso

  /** compose a [[Fold_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose a [[Fold_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asLens

  /** compose a [[Fold_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose a [[Fold_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asPrism

  /** compose a [[Fold_]] with a [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose a [[Fold_]] with a [[AnAffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.viewOrModify(_).fold(const(Monoid.empty[R]), forget.runForget)))
  }

  /** compose a [[Fold_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose a [[Fold_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asTraversal

  /** compose a [[Fold_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold

  /** compose a [[Fold_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  private[proptics] def hasOrHasnt[R: Heyting](s: S)(r: R): R = foldMap(s)(const(Disj(r))).runDisj

  private[proptics] def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private[proptics] def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldr[Option[A]](s)(None)((a, op) => f(a, op.getOrElse(a)).some)
}

object Fold_ {

  /** create a polymorphic [[Fold_]] from Rank2TypeFoldLike encoding */
  private[proptics] def apply[S, T, A, B](f: Rank2TypeFoldLike[S, T, A, B]): Fold_[S, T, A, B] = new Fold_[S, T, A, B] { self =>
    override def apply[R](forget: Forget[R, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] = f(forget)
  }

  /** create a polymorphic [[Fold_]] from a getter function */
  def apply[S, T, A, B](get: S => A)(implicit ev: DummyImplicit): Fold_[S, T, A, B] =
    Fold_(fromGetRank2TypeFoldLike[S, T, A, B](get))

  /** create a polymorphic [[Fold_]] using a predicate to filter out elements of future optics composed with this [[Fold_]] */
  def filtered[P[_, _], A](predicate: A => Boolean): Fold_[A, A, A, A] =
    Fold_[A, A, A, A](new Rank2TypeFoldLike[A, A, A, A] {
      override def apply[R](forget: Forget[R, A, A])(implicit ev: Monoid[R]): Forget[R, A, A] =
        Forget { a =>
          if (predicate(a)) forget.runForget(a)
          else ev.empty
        }
    })

  /** create a polymorphic [[Fold_]] by replicating the elements of a fold */
  def replicate[A, B, T](i: Int): Fold_[A, B, A, T] = Fold_(replicateRank2TypeFoldLike[A, B, T](i))

  /** create a polymorphic [[Fold_]] from [[Foldable]] */
  def fromFoldable[F[_], A, B, T](implicit ev0: Foldable[F]): Fold_[F[A], B, A, T] = new Fold_[F[A], B, A, T] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, T]): Forget[R, F[A], B] =
      Forget(ev0.foldMap(_)(forget.runForget))

    override def foldMap[R](s: F[A])(f: A => R)(implicit ev: Monoid[R]): R = ev0.foldMap(s)(f)
  }

  /** create a polymorphic [[Fold_]] using an unfold function */
  def unfold[S, T, A, B](f: S => Option[(A, S)]): Fold_[S, T, A, B] = Fold_(unfoldRank2TypeFoldLike[S, T, A, B](f))

  private[proptics] def fromGetRank2TypeFoldLike[S, T, A, B](get: S => A): Rank2TypeFoldLike[S, T, A, B] = new Rank2TypeFoldLike[S, T, A, B] {
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = liftForget[R, S, T, A, B](get)(forget)
  }

  private[proptics] def replicateRank2TypeFoldLike[A, B, T](i: Int): Rank2TypeFoldLike[A, B, A, T] = new Rank2TypeFoldLike[A, B, A, T] {
    override def apply[R: Monoid](forget: Forget[R, A, T]): Forget[R, A, B] = {
      def go[RR](i: Int, r: RR)(implicit ev: Monoid[RR]): RR = (i, r) match {
        case (0, _) => ev.empty
        case (n, x) => x |+| go(n - 1, x)
      }

      Forget(a => go[R](i, forget.runForget(a)))
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

object Fold {

  /** create a monomorphic [[Fold]] from a getter function */
  def apply[S, A](f: S => A): Fold[S, A] = Fold_(f)

  /** create a monomorphic [[Fold]] using a predicate to filter out elements of future optics composed with this [[Fold_]] */
  def filtered[A](predicate: A => Boolean): Fold[A, A] = Fold_.filtered(predicate)

  /** create a monomorphic [[Fold]] by replicating the elements of a fold */
  def replicate[A, T](i: Int): Fold_[A, A, A, T] = Fold_.replicate(i)

  /** create a monomorphic [[Fold]] from [[Foldable]] */
  def fromFoldable[F[_]: Foldable, A, T]: Fold_[F[A], A, A, T] = Fold_.fromFoldable

  /** create a monomorphic [[Fold]] using an unfold function */
  def unfold[S, A](f: S => Option[(A, S)]): Fold[S, A] = Fold_.unfold(f)
}
