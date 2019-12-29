package optics.syntax

import algebra.lattice.Heyting
import cats.kernel.Semigroup
import cats.mtl.MonadState
import optics.Optic
import spire.algebra.{Field, Ring, Semiring}

import scala.Function.const

object SetterSyntax {
  implicit class SetterOps[S, T, A, B](val optic: Optic[* => *, S, T, A, B]) extends AnyVal {
    def over(f: A => B): S => T = optic(f)

    def set(b: B): S => T = over(const(b))
  }

  implicit class SetterSTAAOps[S, T, A](val setter: Optic[* => *, S, T, A, A]) extends AnyVal {
    def addOver(a: A)(implicit ev: Semiring[A]): S => T = setter.over(ev.additive.combine(_, a))

    def mulOver(a: A)(implicit ev: Semiring[A]): S => T = setter.over(ev.multiplicative.combine(_, a))

    def subOver(a: A)(implicit ev: Ring[A]): S => T = setter.over(ev.minus(_, a))

    def divOver(a: A)(implicit ev: Field[A]): S => T = setter.over(ev.div(_, a))

    def disjOver(a: A)(implicit ev: Heyting[A]): S => T = setter.over(ev.or(_, a))

    def conjOver(a: A)(implicit ev: Heyting[A]): S => T = setter.over(ev.and(_, a))

    def appendOver(a: A)(implicit ev: Semigroup[A]): S => T = setter.over(ev.combine(_, a))
  }

  implicit class SetterSSABOps[S, A, B](val setter: Optic[* => *, S, S, A, B]) extends AnyVal {
    def assign[F[_]](b: B)(implicit ev: MonadState[F, S]): F[Unit] = modifying(const(b))

    def modifying[F[_]](f: A => B)(implicit ev: MonadState[F, S]): F[Unit] =
      ev.monad.void(ev.modify(setter.over(f)(_)))
  }

  implicit class SetterSAOps[S, A](val setter: Optic[* => *, S, S, A, A]) extends AnyVal {
    def addModifying[F[_]](a: A)(implicit ev: MonadState[F, S], ev2: Semiring[A]): F[Unit] =
      setter.modifying(ev2.additive.combine(a, _))

    def mulModifying[F[_]](a: A)(implicit ev: MonadState[F, S], ev2: Semiring[A]): F[Unit] =
      setter.modifying(ev2.multiplicative.combine(a, _))

    def subModifying[F[_]](a: A)(implicit ev: MonadState[F, S], ev2: Ring[A]): F[Unit] =
      setter.modifying(ev2.minus(a, _))

    def divModifying[F[_]](a: A)(implicit ev: MonadState[F, S], ev2: Field[A]): F[Unit] =
      setter.modifying(ev2.div(a, _))

    def disjModifying[F[_]](a: A)(implicit ev: MonadState[F, S], ev2: Heyting[A]): F[Unit] =
      setter.modifying(ev2.or(a, _))

    def conjModifying[F[_]](a: A)(implicit ev: MonadState[F, S], ev2: Heyting[A]): F[Unit] =
      setter.modifying(ev2.and(a, _))

    def conjModifying[F[_]](a: A)(implicit ev: MonadState[F, S], ev2: Semigroup[A]): F[Unit] =
      setter.modifying(ev2.combine(a, _))
  }

  implicit class SetterSTAOptionB[S, T, A, B](val setter: Optic[* => *, S, T, A, Option[B]]) extends AnyVal {
    def setJust(b: B): S => T = setter.set(Some(b))
  }

  implicit class SetterSSAOptionB[F[_], S, A, B](val setter: Optic[* => *, S, S, A, Option[B]]) extends AnyVal {
    def assignJust(b: B)(implicit ev: MonadState[F, S]): F[Unit] = setter.assign(Some(b))
  }
}
