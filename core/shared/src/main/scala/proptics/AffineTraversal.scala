package proptics

import cats.{Applicative, Eq, Monoid}
import cats.arrow.Strong
import cats.data.Const
import cats.syntax.option._
import cats.syntax.either._
import cats.syntax.eq._
import cats.instances.function._
import cats.syntax.apply._
import proptics.internal.{Forget, RunBazaar, Wander, Zipping}
import proptics.instances.boolean._
import proptics.newtype.{Conj, Disj, First, Newtype}
import proptics.profunctor.{Choice, Star}
import proptics.rank2types.Rank2TypeAffineTraversalLike
import spire.algebra.lattice.Heyting

import scala.Function.const

/**
  * An affine traversal has at most one focus, but is not a [[Prism_]]
  *
  * @tparam S the source of an [[AffineTraversal_]]
  * @tparam T the modified source of an [[AffineTraversal_]]
  * @tparam A the focus of an [[AffineTraversal_]]
  * @tparam B the modified focus of an [[AffineTraversal_]]
  */
abstract class AffineTraversal_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T]

  /** view an optional focus of an [[AffineTraversal_]] */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  /** set the modified focus of an [[AffineTraversal_]] */
  def set(b: B): S => T = over(const(b))

  /** set the focus of an [[AffineTraversal_]] conditionally if it is not None */
  def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus type of an [[AffineTraversal_]] using a function, resulting in a change of type to the full structure  */
  def over(f: A => B): S => T = self(f)

  /** modify the focus of an [[AffineTraversal_]] using a function conditionally if it is not None, resulting in a change of type to the full structure  */
  def overOption(f: A => B): S => Option[T] = s => preview(s).map(a => set(f(a))(s))

  /** synonym for [[traverse]], flipped  */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AffineTraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure  */
  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  /** tests whether there is no focus or a predicate holds for the focus of a [[Prism_]] */
  def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** tests whether there is no focus or a predicate holds for the focus of an [[AffineTraversal_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** tests whether a predicate holds for the focus of an [[AffineTraversal_]] */
  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](_)(f)

  /** tests whether a predicate does not hold for the focus of an [[AffineTraversal_]] */
  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** tests whether the focus of an [[AffineTraversal_]] contains a given value */
  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** tests whether the focus of an [[AffineTraversal_]] does not contain a given value */
  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  /** check if the [[AffineTraversal_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[AffineTraversal_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** finds if the focus of an [[AffineTraversal_]] is satisfying a predicate. */
  def find(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  /** zip two sources of an [[AffineTraversal_]] together provided a binary operation which modify the focus type of a [[Prism_]] */
  def zipWith[F[_]](f: A => A => B): S => S => T = self(Zipping(f)).runZipping

  /** compose an [[AffineTraversal_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab)(ev1))
  }

  /** compose an [[AffineTraversal_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self compose other.asIso

  /** compose an [[AffineTraversal_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))
  }

  /** compose an [[AffineTraversal_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self compose other.asLens

  /** compose an [[AffineTraversal_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): AffineTraversal_[S, T, C, D] = new AffineTraversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = self(other(pab))
  }

  /** compose an [[AffineTraversal_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): AffineTraversal_[S, T, C, D] = self compose other.asPrism

  /** compose an [[AffineTraversal_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = self(other(pab))
  }

  /** compose an [[AffineTraversal_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[AffineTraversal_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose an [[AffineTraversal_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold

  /** compose an [[AffineTraversal_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] = self(other(forget))
  }

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object AffineTraversal_ {

  /** create a polymorphic [[AffineTraversal_]] from Rank2TypeAffineTraversalLike encoding */
  private[proptics] def apply[S, T, A, B](f: Rank2TypeAffineTraversalLike[S, T, A, B]): AffineTraversal_[S, T, A, B] = new AffineTraversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = f(pab)
  }

  /** create a polymorphic [[AffineTraversal_]] from a getter/setter pair */
  def apply[S, T, A, B](get: S => Either[T, A])(_set: S => B => T): AffineTraversal_[S, T, A, B] =
    AffineTraversal_((get, _set).mapN(Tuple2.apply))

  /** create a polymorphic [[AffineTraversal_]] from a combined getter/setter */
  def apply[S, T, A, B](to: S => (Either[T, A], B => T)): AffineTraversal_[S, T, A, B] = new AffineTraversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = {
      val eitherPab = ev1.first[Either[T, A], Either[T, B], B => T](ev0.right(pab))

      ev0.dimap(eitherPab)(to) { case (f, b) => f.fold(identity, b) }
    }
  }
}

object AffineTraversal {

  /** create a monomorphic [[AffineTraversal]], using a preview, an operation which returns an [[Option]] */
  def fromOption[S, A](preview: S => Option[A])(review: S => A => S): AffineTraversal[S, A] =
    AffineTraversal { s: S => preview(s).fold(s.asLeft[A])(_.asRight[S]) }(review)

  /** create a momnomorphic [[AffineTraversal]] from a getter/setter pair */
  def apply[S, A](get: S => Either[S, A])(set: S => A => S): AffineTraversal[S, A] = AffineTraversal_(get)(set)

  /** create a monomorphic [[AffineTraversal]] from a combined getter/setter */
  def apply[S, A](to: S => (Either[S, A], A => S)): AffineTraversal[S, A] = AffineTraversal_(to)
}
