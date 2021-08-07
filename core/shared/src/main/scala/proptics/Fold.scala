package proptics

import scala.Function.const
import scala.reflect.ClassTag

import cats.data.State
import cats.syntax.bifoldable._
import cats.syntax.eq._
import cats.syntax.monoid._
import cats.syntax.option._
import cats.{Bifoldable, Eq, Foldable, Monoid, Order}
import spire.algebra.lattice.Heyting
import spire.algebra.{MultiplicativeMonoid, Semiring}
import spire.std.boolean._

import proptics.data.First._
import proptics.data._
import proptics.internal.{Forget, Indexed}
import proptics.rank2types.{Rank2TypeFoldLike, Rank2TypeIndexedFoldLike}
import proptics.syntax.fold._
import proptics.syntax.function._

/** A [[Fold_]] is a generalization of something Foldable. It describes how to retrieve multiple values.
  *
  * A [[Fold_]] is similar to a [[Traversal_]], but it cannot modify its foci.
  *
  * A [[Fold_]] is an Optic with fixed type [[proptics.internal.Forget]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of a [[Fold_]]
  * @tparam T the modified source of a [[Fold_]]
  * @tparam A the foci of a [[Fold_]]
  * @tparam B the modified foci of a [[Fold_]]
  */
abstract class Fold_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T]

  /** synonym to [[fold]] */
  final def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  /** collect all the foci of a [[Fold_]] into aList */
  final def viewAll(s: S): List[A] = foldMap(s)(List(_))

  /** view the first focus of a [[Fold_]], if there is any */
  final def preview(s: S): Option[A] = foldMap(s)(a => First(a.some)).runFirst

  /** map each focus of a [[Fold_]] to a [[cats.Monoid]], and combine the results */
  def foldMap[R: Monoid](s: S)(f: A => R): R = self(Forget(f)).runForget(s)

  /** fold the foci of a [[Fold_]] using a [[cats.Monoid]] */
  final def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  /** fold the foci of a [[Fold_]] using a binary operator, going right to left */
  final def foldRight[R](s: S)(r: R)(f: (A, R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of a [[Fold_]] using a binary operator, going left to right */
  final def foldLeft[R](s: S)(r: R)(f: (R, A) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** the sum of all foci of a [[Fold_]] */
  final def sum(s: S)(implicit ev: Semiring[A]): A = foldMap(s)(Additive.apply).runAdditive

  /** the product of all foci of a [[Fold_]] */
  final def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A =
    foldMap(s)(Multiplicative.apply).runMultiplicative

  /** test whether there is no focus or a predicate holds for all foci of a [[Fold_]] */
  final def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for all foci of a [[Fold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def forall[R: Heyting](s: S)(f: A => R): R = foldMap(s)(Conj[R] _ compose f).runConj

  /** return the result of a conjunction of all foci of a [[Fold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(identity)

  /** returns the result of a disjunction of all foci of a [[Fold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def or(s: S)(implicit ev: Heyting[A]): A = any[A](s)(identity)

  /** test whether a predicate holds for any focus of a [[Fold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def any[R: Heyting](s: S)(f: A => R): R = foldMap(s)(Disj[R] _ compose f).runDisj

  /** test whether a predicate holds for any foci of a [[Fold_]] */
  final def exists(f: A => Boolean): S => Boolean = any[Boolean](_)(f)

  /** test whether a predicate does not hold for the foci of a [[Fold_]] */
  final def notExists(f: A => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a [[Fold_]] contains a specific focus */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether a [[Fold_]] does not contain a specific focus */
  final def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** check if the [[Fold_]] does not contain a focus */
  final def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[Fold_]] contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** the number of foci of a [[Fold_]] */
  final def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of a [[Fold_]] that satisfies a predicate, if there is any */
  final def find(f: A => Boolean): S => Option[A] =
    foldRight[Option[A]](_)(None)((a, op) => op.fold(if (f(a)) a.some else None)(Some[A]))

  /** find the first focus of a [[Fold_]], if there is any. Synonym for preview */
  final def first(s: S): Option[A] = preview(s)

  /** find the last focus of a [[Fold_]], if there is any */
  final def last(s: S): Option[A] = foldMap(s)(a => Last(a.some)).runLast

  /** the minimum of all foci of a [[Fold_]], if there is any */
  final def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of a [[Fold_]], if there is any */
  final def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of a [[Fold_]] into an Array */
  final def toArray[AA >: A](s: S)(implicit ev: ClassTag[AA]): Array[AA] = toList(s).toArray

  /** synonym to [[viewAll]] */
  final def toList(s: S): List[A] = viewAll(s)

  /** collect all the foci of a [[Fold_]] in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, List[A]] = ev.inspect(viewAll)

  /** convert a [[Fold_]] to an [[IndexedFold_]] by using the integer positions as indices */
  final def asIndexableFold: IndexedFold_[Int, S, T, A, B] =
    IndexedFold_(new Rank2TypeIndexedFoldLike[Int, S, T, A, B] {
      override def apply[R](indexed: Indexed[Forget[R, *, *], Int, A, B])(implicit ev1: Monoid[R]): Forget[R, S, T] = {
        val runForget: ((A, Int)) => R = indexed.runIndex.runForget
        Forget { s =>
          self.foldLeft(s)((0, ev1.empty)) { case ((i, r), a) => (i + 1, r |+| runForget((a, i))) }._2
        }
      }
    })

  /** compose this [[Fold_]] with a function lifted to a [[Getter_]], having this [[Fold_]] applied last */
  final def to[C, D](f: A => C): Fold_[S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose this [[Fold_]] with an [[Iso_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Iso_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      self(other(forget)(Forget.profunctorForget))
  }

  /** compose this [[Fold_]] with an [[Iso_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Iso_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with an [[AnIso_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: AnIso_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => self.foldMap(s)(forget.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[AnIso_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AnIso_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with a [[Lens_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Lens_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[Lens_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Lens_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with an [[ALens_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: ALens_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => self.foldMap(s)(forget.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[ALens_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: ALens_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with a [[Prism_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Prism_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[Prism_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Prism_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.preview(_).fold(Monoid[R].empty)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[APrism_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: APrism_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => self.foldMap(s)(other.viewOrModify(_).fold(const(Monoid[R].empty), forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[APrism_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: APrism_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.preview(_).fold(Monoid[R].empty)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[AffineTraversal_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[AffineTraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.preview(_).fold(Monoid[R].empty)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[AnAffineTraversal_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.viewOrModify(_).fold(const(Monoid.empty[R]), forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[AnAffineTraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.preview(_).fold(Monoid[R].empty)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[Traversal_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Traversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[Traversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Traversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[ATraversal_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[ATraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: ATraversal_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with a [[Getter_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose this [[Fold_]] with a [[Getter_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Getter_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose this [[Fold_]] with a [[Fold_]], having this [[Fold_]] applied last */
  final def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  /** compose this [[Fold_]] with a [[Fold_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[Fold_]] with an [[IndexedLens_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => self.foldMap(s)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[IndexedLens_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[Fold_]] with an [[AnIndexedLens_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[AnIndexedLens_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[Fold_]] with an [[IndexedTraversal_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[Fold_]] with an [[IndexedTraversal_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget {
        other.foldMap(_) { case (s, i) => self.foldMap(s)(a => indexed.runIndex.runForget((a, i))) }
      }
  }

  /** compose this [[Fold_]] with an [[IndexedGetter_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[Fold_]] with an [[IndexedGetter_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[Fold_]] with an [[IndexedFold_]], having this [[Fold_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[Fold_]] with an [[IndexedFold_]], having this [[Fold_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget {
        other.foldMap(_) { case (s, i) => self.foldMap(s)(a => indexed.runIndex.runForget((a, i))) }
      }
  }

  private[proptics] def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldLeft[Option[A]](s)(None)((op, a) => f(a, op.getOrElse(a)).some)
}

object Fold_ {
  /** create a polymorphic [[Fold_]] from Rank2TypeFoldLike encoding */
  private[proptics] def apply[S, T, A, B](f: Rank2TypeFoldLike[S, T, A, B]): Fold_[S, T, A, B] = new Fold_[S, T, A, B] { self =>
    override def apply[R](forget: Forget[R, A, B])(implicit ev: Monoid[R]): Forget[R, S, T] = f(forget)
  }

  /** create a polymorphic [[Fold_]] from a getter function */
  final def apply[S, T, A, B](get: S => A): Fold_[S, T, A, B] =
    Fold_(fromGetRank2TypeFoldLike[S, T, A, B](get))

  /** create a polymorphic [[Fold_]] using a predicate to filter out elements of future optics composed with this [[Fold_]] */
  final def filter[A](predicate: A => Boolean): Fold_[A, A, A, A] =
    Fold_[A, A, A, A](new Rank2TypeFoldLike[A, A, A, A] {
      override def apply[R](forget: Forget[R, A, A])(implicit ev: Monoid[R]): Forget[R, A, A] =
        Forget { a =>
          if (predicate(a)) forget.runForget(a)
          else ev.empty
        }
    })

  /** create a polymorphic [[Fold_]] from [[cats.Foldable]] */
  final def fromFoldable[F[_], A, B, T](implicit ev0: Foldable[F]): Fold_[F[A], B, A, T] = new Fold_[F[A], B, A, T] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, T]): Forget[R, F[A], B] =
      Forget(ev0.foldMap(_)(forget.runForget))

    override def foldMap[R](s: F[A])(f: A => R)(implicit ev: Monoid[R]): R = ev0.foldMap(s)(f)
  }

  /** fold both parts of a [[cats.Bifoldable]] with matching types */
  final def both[G[_, _]: Bifoldable, A, B]: Fold_[G[A, A], G[B, B], A, B] =
    Fold_(new Rank2TypeFoldLike[G[A, A], G[B, B], A, B] {
      override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, G[A, A], G[B, B]] = {
        val fold: (R, A) => R = (r, a) => r |+| forget.runForget(a)

        Forget(_.bifoldLeft(Monoid[R].empty)(fold, fold))
      }
    })

  /** polymorphic identity of a [[Fold_]] */
  final def id[S, T]: Fold_[S, T, S, T] = Fold_[S, T, S, T] { s: S => s }

  private[proptics] def fromGetRank2TypeFoldLike[S, T, A, B](get: S => A): Rank2TypeFoldLike[S, T, A, B] = new Rank2TypeFoldLike[S, T, A, B] {
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = liftForget[R, S, T, A, B](get)(forget)
  }

  private[proptics] def liftForget[R, S, T, A, B](f: S => A): Forget[R, A, B] => Forget[R, S, T] =
    forget => Forget(forget.runForget compose f)

  /** implicit conversion from [[Lens_]] to [[Fold_]] */
  implicit final def lensToFold[S, T, A, B](lens: Lens_[S, T, A, B]): Fold_[S, T, A, B] = lens.asFold

  /** implicit conversion from [[ALens_]] to [[Fold_]] */
  implicit final def aLensToFold[S, T, A, B](aLens: ALens_[S, T, A, B]): Fold_[S, T, A, B] = aLens.asFold

  /** implicit conversion from [[Prism_]] to [[Fold_]] */
  implicit final def prismToFold[S, T, A, B](prism: Prism_[S, T, A, B]): Fold_[S, T, A, B] = prism.asFold

  /** implicit conversion from [[APrism_]] to [[Fold_]] */
  implicit final def aPrismToFold[S, T, A, B](aPrism: APrism_[S, T, A, B]): Fold_[S, T, A, B] = aPrism.asFold

  /** implicit conversion from [[AffineTraversal_]] to [[Fold_]] */
  implicit final def affineTraversalToFold[S, T, A, B](affineTraversal: AffineTraversal_[S, T, A, B]): Fold_[S, T, A, B] = affineTraversal.asFold

  /** implicit conversion from [[AnAffineTraversal_]] to [[Fold_]] */
  implicit final def anAffineTraversalToFold[S, T, A, B](anAffineTraversal: AnAffineTraversal_[S, T, A, B]): Fold_[S, T, A, B] = anAffineTraversal.asFold

  /** implicit conversion from [[Traversal_]] to [[Fold_]] */
  implicit final def traversalToFold[S, T, A, B](traversal: Traversal_[S, T, A, B]): Fold_[S, T, A, B] = traversal.asFold

  /** implicit conversion from [[ATraversal_]] to [[Fold_]] */
  implicit final def aTraversalToFold[S, T, A, B](aTraversal: ATraversal_[S, T, A, B]): Fold_[S, T, A, B] = aTraversal.asFold

  /** implicit conversion from [[Getter_]] to [[Fold_]] */
  implicit final def getterToFold[S, T, A, B](getter: Getter_[S, T, A, B]): Fold_[S, T, A, B] = getter.asFold
}

object Fold {
  /** create a monomorphic [[Fold]] from a getter function */
  final def apply[S, A](f: S => A): Fold[S, A] = Fold_(f)

  /** create a monomorphic [[Fold]] using a predicate to filter out elements of future optics composed with this [[Fold_]] */
  final def filter[A](predicate: A => Boolean): Fold[A, A] = Fold_.filter(predicate)

  /** create a monomorphic [[Fold]] using a [[Fold]] to filter out elements of future optics composed with this [[Fold_]] */
  final def filter[A, B](fold: Fold[A, B]): Fold[A, A] =
    Fold_[A, A, A, A](new Rank2TypeFoldLike[A, A, A, A] {
      override def apply[R: Monoid](forget: Forget[R, A, A]): Forget[R, A, A] =
        Forget[R, A, A] { a =>
          fold.preview(a).fold(Monoid[R].empty)(const(forget.runForget(a)))
        }
    })

  /** create a monomorphic [[Fold]] from [[cats.Foldable]] */
  final def fromFoldable[F[_]: Foldable, A]: Fold[F[A], A] = Fold_.fromFoldable

  /** monomorphic identity of a [[Fold]] */
  final def id[S]: Fold[S, S] = Fold_.id[S, S]

  /** create a monomorphic [[Fold]] that narrows the focus to a single element */
  final def elementAt[F[_]: Foldable, A](i: Int): Fold[F[A], A] = Fold.fromFoldable[F, A].elementAt(i)

  /** create a monomorphic [[Fold]] that selects the first n elements of a Foldable */
  final def take[G[_]: Foldable, A](i: Int): Fold[G[A], A] = Fold.fromFoldable[G, A].take(i)

  /** create a monomorphic [[Fold]] that selects all elements of a Traverse except the first n ones */
  final def drop[G[_]: Foldable, A](i: Int): Fold[G[A], A] = Fold.fromFoldable[G, A].drop(i)

  /** create a monomorphic [[Fold]] that takes the longest prefix of elements of a Foldable that satisfy a predicate */
  final def takeWhile[G[_]: Foldable, A](predicate: A => Boolean): Fold[G[A], A] =
    Fold.fromFoldable[G, A].takeWhile(predicate)

  /** create a monomorphic [[Fold]] that drop longest prefix of elements of a Foldable that satisfy a predicate */
  final def dropWhile[G[_]: Foldable, A](predicate: A => Boolean): Fold[G[A], A] =
    Fold.fromFoldable[G, A].dropWhile(predicate)

  /** check to see if a [[Fold]] matches one or more entries */
  final def has[S, A](fold: Fold[S, A]): S => Boolean = fold.nonEmpty
}
