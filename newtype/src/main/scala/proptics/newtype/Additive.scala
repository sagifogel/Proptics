package proptics.newtype

import cats.Monoid
import cats.Semigroup
import cats.syntax.order._
import cats.syntax.show._
import spire.syntax.semiring._
import cats.{Applicative, Apply, Eq, FlatMap, Functor, Monad, Order, Show}
import spire.algebra.Semiring

import scala.annotation.tailrec

/** [[Monoid]] and [[Semigroup]] for [[Semiring]]s under addition */
final case class Additive[A](runAdditive: A) extends AnyVal

abstract class AdditiveInstances {
  implicit final def eqAdditive[A: Eq]: Eq[Additive[A]] = new Eq[Additive[A]] {
    override def eqv(x: Additive[A], y: Additive[A]): Boolean = x.runAdditive === y.runAdditive
  }

  implicit final def ordAdditive[A: Order]: Order[Additive[A]] = new Order[Additive[A]] {
    override def compare(x: Additive[A], y: Additive[A]): Int = x.runAdditive.compare(y.runAdditive)
  }

  implicit final def showAdditive[A: Show]: Show[Additive[A]] = new Show[Additive[A]] {
    override def show(t: Additive[A]): String = s"(Additive ${t.runAdditive.show})"
  }

  implicit final def semigroupAdditive[A: Semiring]: Semigroup[Additive[A]] = new Semigroup[Additive[A]] {
    override def combine(x: Additive[A], y: Additive[A]): Additive[A] = Additive(x.runAdditive + y.runAdditive)
  }

  implicit final def monoidAdditive[A](implicit ev: Semiring[A]): Monoid[Additive[A]] = new Monoid[Additive[A]] {
    def empty: Additive[A] = Additive(ev.zero)

    def combine(x: Additive[A], y: Additive[A]): Additive[A] = semigroupAdditive.combine(x, y)
  }

  implicit final def functorAdditive: Functor[Additive] = new Functor[Additive] {
    override def map[A, B](fa: Additive[A])(f: A => B): Additive[B] = Additive(f(fa.runAdditive))
  }

  implicit final def applyAdditive: Apply[Additive] = new Apply[Additive] {
    override def ap[A, B](ff: Additive[A => B])(fa: Additive[A]): Additive[B] = Additive(ff.runAdditive(fa.runAdditive))

    override def map[A, B](fa: Additive[A])(f: A => B): Additive[B] = functorAdditive.fmap(fa)(f)
  }

  implicit final def applicativeAdditive: Applicative[Additive] = new Applicative[Additive] {
    override def pure[A](x: A): Additive[A] = Additive(x)

    override def ap[A, B](ff: Additive[A => B])(fa: Additive[A]): Additive[B] = applyAdditive.ap(ff)(fa)
  }

  implicit final def bindAdditive: FlatMap[Additive] = new FlatMap[Additive] {
    override def flatMap[A, B](fa: Additive[A])(f: A => Additive[B]): Additive[B] = f(fa.runAdditive)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Additive[Either[A, B]]): Additive[B] =
      f(a).runAdditive match {
        case Right(value) => Additive(value)
        case Left(value)  => tailRecM(value)(f)
      }

    override def map[A, B](fa: Additive[A])(f: A => B): Additive[B] = functorAdditive.fmap(fa)(f)
  }

  implicit final def monadAdditive: Monad[Additive] = new Monad[Additive] {
    override def pure[A](x: A): Additive[A] = applicativeAdditive.pure(x)

    override def flatMap[A, B](fa: Additive[A])(f: A => Additive[B]): Additive[B] = bindAdditive.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Additive[scala.Either[A, B]]): Additive[B] = bindAdditive.tailRecM(a)(f)
  }
}

object Additive extends AdditiveInstances
