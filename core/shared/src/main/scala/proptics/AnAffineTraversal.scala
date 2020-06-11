package proptics

import cats.data.Const
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid}
import proptics.instances.boolean._
import proptics.internal.Stall
import proptics.newtype.{Conj, Disj, First, Newtype}
import spire.algebra.lattice.Heyting

import scala.Function.const

abstract class AnAffineTraversal_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(pab: Stall[A, B, A, B]): Stall[A, B, S, T]

  /** view an optional focus of an [[AnAffineTraversal_]] */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  /** set the modified focus of an [[AnAffineTraversal_]] */
  def set(b: B): S => T = over(const(b))

  /** set the focus of an [[AnAffineTraversal_]] conditionally if it is not None */
  def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus type of an [[AnAffineTraversal_]] using a function, resulting in a change of type to the full structure  */
  def over(f: A => B): S => T = overF[Id](f)

  /** modify the focus of a [[AnAffineTraversal_]] using a function conditionally if it is not None, resulting in a change of type to the full structure  */
  def overOption(f: A => B): S => Option[T] = s => preview(s).map(a => set(f(a))(s))

  /** synonym for [[traverse]], flipped  */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AnAffineTraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure  */
  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = {
    val stall = toStall

    stall
      .getOrModify(s)
      .fold(Applicative[F].pure, a => Applicative[F].map(f(a))(stall.set(s)(_)))
  }

  /** test whether there is no focus or a predicate holds for the focus of a [[Prism_]] */
  def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for the focus of an [[AnAffineTraversal_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** test whether a predicate holds for the focus of an [[AnAffineTraversal_]] */
  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](_)(f)

  /** test whether a predicate does not hold for the focus of an [[AnAffineTraversal_]] */
  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of an [[AnAffineTraversal_]] contains a given value */
  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus of an [[AnAffineTraversal_]] does not contain a given value */
  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  /** check if the [[AnAffineTraversal_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[AnAffineTraversal_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** find if the focus of an [[AnAffineTraversal_]] is satisfying a predicate. */
  def find(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  /** convert an [[AnAffineTraversal_]] to the pair of functions that characterize it */
  def withAffineTraversal[R](f: (S => Either[T, A]) => (S => B => T) => R): R = {
    val stall = toStall

    f(stall.getOrModify)(stall.set)
  }

  /** transform an [[AnAffineTraversal_]] to an [[AffineTraversal_]] */
  def asAffineTraversal: AffineTraversal_[S, T, A, B] = withAffineTraversal(AffineTraversal_[S, T, A, B])

  /** compose an [[AffineTraversal_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] = new AnAffineTraversal_[S, T, C, D] {
    override private[proptics] def apply(pab: Stall[C, D, C, D]): Stall[C, D, S, T] =
      Stall(
        s =>
          self.toStall
            .getOrModify(s)
            .flatMap { c =>
              pab.getOrModify(other.view(c)).leftMap(d => self.set(other.review(d))(s))
            },
        s => d => self.over(other.set(d))(s)
      )
  }

  /** compose an [[AffineTraversal_]] with an [[Iso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] = self compose other.asIso

  private def toStall: Stall[A, B, S, T] = self(Stall(_.asRight[B], const(identity[B])))

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

}

object AnAffineTraversal_ {

  /** create a polymorphic [[AnAffineTraversal_]] from an [[AnAffineTraversal_]] encoded in Stall */
  private[proptics] def apply[S, T, A, B](f: Stall[A, B, A, B] => Stall[A, B, S, T]): AnAffineTraversal_[S, T, A, B] = new AnAffineTraversal_[S, T, A, B] { self =>
    override def apply(stall: Stall[A, B, A, B]): Stall[A, B, S, T] = f(stall)
  }

  /** create a polymorphic [[AnAffineTraversal_]] from a getter/setter pair */
  def apply[S, T, A, B](getOrModify: S => Either[T, A])(set: S => B => T): AnAffineTraversal_[S, T, A, B] =
    AnAffineTraversal_ { stall: Stall[A, B, A, B] =>
      Stall(
        s =>
          getOrModify(s) match {
            case Left(t)  => t.asLeft[A]
            case Right(a) => stall.getOrModify(a).leftMap(set(s)(_))
          },
        s => b => getOrModify(s).fold(identity, a => set(s)(stall.set(a)(b)))
      )
    }
}

object AnAffineTraversal {

  /** create a monomorphic [[AnAffineTraversal]], using a preview and a setter functions */
  def fromOption[S, A](preview: S => Option[A])(set: S => A => S): AnAffineTraversal[S, A] =
    AnAffineTraversal { s: S => preview(s).fold(s.asLeft[A])(_.asRight[S]) }(set)

  /**
    * create a monomorphic [[AnAffineTraversal]] from a matcher function that produces an [[Either]] and a setter function
    * <p>
    * the matcher function returns an [[Either]] to allow for type-changing prisms in the case where the input does not match.
    * </p>
    */
  def apply[S, A](getOrModify: S => Either[S, A])(set: S => A => S): AnAffineTraversal[S, A] =
    AnAffineTraversal_(getOrModify)(set)
}
