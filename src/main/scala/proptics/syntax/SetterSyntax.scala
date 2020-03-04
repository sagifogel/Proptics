package proptics.syntax

import algebra.lattice.Heyting
import cats.kernel.Semigroup
import cats.mtl.MonadState
import proptics.Setter
import spire.algebra.{Field, Ring, Semiring}

import scala.Function.const

object SetterSyntax {
  implicit class SetterSTAAOps[S, T, A](val setter: Setter[S, T, A, A]) extends AnyVal {
    def addOver(a: A)(implicit ev0: Semiring[A]): S => T = setter.over(ev0.additive.combine(_, a))

    def mulOver(a: A)(implicit ev0: Semiring[A]): S => T = setter.over(ev0.multiplicative.combine(_, a))

    def subOver(a: A)(implicit ev0: Ring[A]): S => T = setter.over(ev0.minus(_, a))

    def divOver(a: A)(implicit ev0: Field[A]): S => T = setter.over(ev0.div(_, a))

    def disjOver(a: A)(implicit ev0: Heyting[A]): S => T = setter.over(ev0.or(_, a))

    def conjOver(a: A)(implicit ev0: Heyting[A]): S => T = setter.over(ev0.and(_, a))

    def appendOver(a: A)(implicit ev0: Semigroup[A]): S => T = setter.over(ev0.combine(_, a))
  }

  implicit class SetterSSABOps[S, A, B](val setter: Setter[ S, S, A, B]) extends AnyVal {
    def assign[F[_]](b: B)(implicit ev0: MonadState[F, S]): F[Unit] = modifying(const(b))

    def modifying[F[_]](f: A => B)(implicit ev0: MonadState[F, S]): F[Unit] =
      ev0.monad.void(ev0.modify(setter.over(f)(_)))
  }

  implicit class SetterSAOps[S, A](val setter: Setter[S, S, A, A]) extends AnyVal {
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

  implicit class SetterSTAOptionB[S, T, A, B](val setter: Setter[S, T, A, Option[B]]) extends AnyVal {
    def setJust(b: B): S => T = setter.set(Some(b))
  }

  implicit class SetterSSAOptionB[F[_], S, A, B](val setter: Setter[S, S, A, Option[B]]) extends AnyVal {
    def assignJust(b: B)(implicit ev0: MonadState[F, S]): F[Unit] = setter.assign(Some(b))
  }
}
