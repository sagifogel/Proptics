package proptics.syntax

import algebra.lattice.Heyting
import cats.kernel.Semigroup
import cats.mtl.MonadState
import proptics.{Setter, Setter_}
import spire.algebra.{Field, Ring, Semiring}

import scala.Function.const

trait SetterSyntax {
  implicit def setterSTAAOps[S, T, A](setter: Setter_[S, T, A, A]) = SetterSTAAOps(setter)

  implicit def setterSSABOps[S, A, B](setter: Setter_[S, S, A, B]) = SetterSSABOps(setter)

  implicit def setterOps[S, A](setter: Setter[S, A]) = SetterOps(setter)

  implicit def setterSTAOptionB[S, T, A, B](setter: Setter_[S, T, A, Option[B]]) = SetterSTAOptionB(setter)

  implicit def setterSSAOptionB[S, A, B](setter: Setter_[S, S, A, Option[B]]) = SetterSSAOptionB(setter)
}

final case class SetterSTAAOps[S, T, A](private val setter: Setter_[S, T, A, A]) extends AnyVal {
  def addOver(a: A)(implicit ev0: Semiring[A]): S => T = setter.over(ev0.additive.combine(_, a))

  def mulOver(a: A)(implicit ev0: Semiring[A]): S => T = setter.over(ev0.multiplicative.combine(_, a))

  def subOver(a: A)(implicit ev0: Ring[A]): S => T = setter.over(ev0.minus(_, a))

  def divOver(a: A)(implicit ev0: Field[A]): S => T = setter.over(ev0.div(_, a))

  def disjOver(a: A)(implicit ev0: Heyting[A]): S => T = setter.over(ev0.or(_, a))

  def conjOver(a: A)(implicit ev0: Heyting[A]): S => T = setter.over(ev0.and(_, a))

  def appendOver(a: A)(implicit ev0: Semigroup[A]): S => T = setter.over(ev0.combine(_, a))
}

final case class SetterSSABOps[S, A, B](private val setter: Setter_[S, S, A, B]) extends AnyVal {
  def assign[F[_]](b: B)(implicit ev0: MonadState[F, S]): F[Unit] = modifying(const(b))

  def modifying[F[_]](f: A => B)(implicit ev0: MonadState[F, S]): F[Unit] =
    ev0.monad.void(ev0.modify(setter.over(f)(_)))
}

final case class SetterOps[S, A](private val setter: Setter_[S, S, A, A]) extends AnyVal {
  import proptics.syntax.setter._

  def addModifying[F[_]](a: A)(implicit ev0: MonadState[F, S], ev1: Semiring[A]): F[Unit] =
    setter.modifying(ev1.additive.combine(a, _))

  def mulModifying[F[_]](a: A)(implicit ev0: MonadState[F, S], ev1: Semiring[A]): F[Unit] =
    setter.modifying(ev1.multiplicative.combine(a, _))

  def subModifying[F[_]](a: A)(implicit ev0: MonadState[F, S], ev1: Ring[A]): F[Unit] =
    setter.modifying(ev1.minus(a, _))

  def divModifying[F[_]](a: A)(implicit ev0: MonadState[F, S], ev1: Field[A]): F[Unit] =
    setter.modifying(ev1.div(a, _))

  def disjModifying[F[_]](a: A)(implicit ev0: MonadState[F, S], ev1: Heyting[A]): F[Unit] =
    setter.modifying(ev1.or(a, _))

  def conjModifying[F[_]](a: A)(implicit ev0: MonadState[F, S], ev1: Heyting[A]): F[Unit] =
    setter.modifying(ev1.and(a, _))

  def conjModifying[F[_]](a: A)(implicit ev0: MonadState[F, S], ev1: Semigroup[A]): F[Unit] =
    setter.modifying(ev1.combine(a, _))
}

final case class SetterSTAOptionB[S, T, A, B](private val setter: Setter_[S, T, A, Option[B]]) extends AnyVal {
  def setJust(b: B): S => T = setter.set(Some(b))
}

final case class SetterSSAOptionB[S, A, B](private val setter: Setter_[S, S, A, Option[B]]) extends AnyVal {
  import proptics.syntax.setter._

  def assignJust[F[_]](b: B)(implicit ev0: MonadState[F, S]): F[Unit] = setter.assign(Some(b))
}
