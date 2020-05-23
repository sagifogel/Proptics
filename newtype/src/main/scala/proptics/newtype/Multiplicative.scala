package proptics.newtype

import cats.{Applicative, Apply, Eq, FlatMap, Functor, Monad, Order, Show}
import cats.Monoid
import cats.Semigroup
import cats.syntax.order._
import cats.syntax.show._
import spire.algebra.Semiring
import spire.syntax.semiring._

import scala.annotation.tailrec

/** [[Monoid]] and [[Semigroup]] for [[Semiring]]s under multiplication */
final case class Multiplicative[A](runMultiplicative: A) extends AnyVal

abstract class MultiplicativeInstances {
  implicit final def eqMultiplicative[A: Eq]: Eq[Multiplicative[A]] = new Eq[Multiplicative[A]] {
    override def eqv(x: Multiplicative[A], y: Multiplicative[A]): Boolean = x.runMultiplicative === y.runMultiplicative
  }

  implicit final def ordMultiplicative[A: Order]: Order[Multiplicative[A]] = new Order[Multiplicative[A]] {
    override def compare(x: Multiplicative[A], y: Multiplicative[A]): Int = x.runMultiplicative.compare(y.runMultiplicative)
  }

  implicit final def showMultiplicative[A: Show]: Show[Multiplicative[A]] = new Show[Multiplicative[A]] {
    override def show(t: Multiplicative[A]): String = s"(Multiplicative ${t.runMultiplicative.show})"
  }

  implicit final def semigroupMultiplicative[A: Semiring]: Semigroup[Multiplicative[A]] = new Semigroup[Multiplicative[A]] {
    def combine(x: Multiplicative[A], y: Multiplicative[A]): Multiplicative[A] =
      Multiplicative(x.runMultiplicative * y.runMultiplicative)
  }

  implicit final def monoidMultiplicative[A](implicit ev: Semiring[A]): Monoid[Multiplicative[A]] = new Monoid[Multiplicative[A]] {
    def empty: Multiplicative[A] = Multiplicative(ev.zero)

    def combine(x: Multiplicative[A], y: Multiplicative[A]): Multiplicative[A] = semigroupMultiplicative.combine(x, y)
  }

  implicit final def functorMultiplicative: Functor[Multiplicative] = new Functor[Multiplicative] {
    override def map[A, B](fa: Multiplicative[A])(f: A => B): Multiplicative[B] = Multiplicative(f(fa.runMultiplicative))
  }

  implicit final def applyMultiplicative: Apply[Multiplicative] = new Apply[Multiplicative] {
    override def ap[A, B](ff: Multiplicative[A => B])(fa: Multiplicative[A]): Multiplicative[B] =
      Multiplicative(ff.runMultiplicative(fa.runMultiplicative))

    override def map[A, B](fa: Multiplicative[A])(f: A => B): Multiplicative[B] = functorMultiplicative.fmap(fa)(f)
  }

  implicit final def applicativeMultiplicative: Applicative[Multiplicative] = new Applicative[Multiplicative] {
    override def pure[A](x: A): Multiplicative[A] = Multiplicative(x)

    override def ap[A, B](ff: Multiplicative[A => B])(fa: Multiplicative[A]): Multiplicative[B] = applyMultiplicative.ap(ff)(fa)
  }

  implicit final def bindMultiplicative: FlatMap[Multiplicative] = new FlatMap[Multiplicative] {
    override def flatMap[A, B](fa: Multiplicative[A])(f: A => Multiplicative[B]): Multiplicative[B] = f(fa.runMultiplicative)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Multiplicative[Either[A, B]]): Multiplicative[B] =
      f(a).runMultiplicative match {
        case Right(value) => Multiplicative(value)
        case Left(value)  => tailRecM(value)(f)
      }

    override def map[A, B](fa: Multiplicative[A])(f: A => B): Multiplicative[B] = functorMultiplicative.fmap(fa)(f)
  }

  implicit final def monadMultiplicative: Monad[Multiplicative] = new Monad[Multiplicative] {
    override def pure[A](x: A): Multiplicative[A] = applicativeMultiplicative.pure(x)

    override def flatMap[A, B](fa: Multiplicative[A])(f: A => Multiplicative[B]): Multiplicative[B] = bindMultiplicative.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Multiplicative[scala.Either[A, B]]): Multiplicative[B] = bindMultiplicative.tailRecM(a)(f)
  }
}

object Multiplicative extends MultiplicativeInstances
