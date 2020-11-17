package proptics.newtype

import scala.annotation.tailrec

import cats.syntax.order._
import cats.syntax.show._
import cats.{Applicative, Apply, Eq, FlatMap, Functor, Monad, Monoid, Order, Semigroup, Show}
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

/** [[Monoid]] and [[Semigroup]] for disjunction */
final case class Disj[A](runDisj: A) extends AnyVal

abstract class DisjInstances {
  implicit final def eqDisj[A: Eq]: Eq[Disj[A]] = new Eq[Disj[A]] {
    override def eqv(x: Disj[A], y: Disj[A]): Boolean = x.runDisj === y.runDisj
  }

  implicit final def ordDisj[A: Order]: Order[Disj[A]] = new Order[Disj[A]] {
    override def compare(x: Disj[A], y: Disj[A]): Int = x.runDisj.compare(y.runDisj)
  }

  implicit final def showDisj[A: Show]: Show[Disj[A]] = new Show[Disj[A]] {
    override def show(t: Disj[A]): String = s"(Disj ${t.runDisj.show})"
  }

  implicit final def semigroupDisj[A](implicit ev: Heyting[A]): Semigroup[Disj[A]] = new Semigroup[Disj[A]] {
    override def combine(x: Disj[A], y: Disj[A]): Disj[A] = Disj(ev.or(x.runDisj, y.runDisj))
  }

  implicit final def monoidDisj[A](implicit ev: Heyting[A]): Monoid[Disj[A]] = new Monoid[Disj[A]] {
    override def empty: Disj[A] = Disj(ev.zero)

    override def combine(x: Disj[A], y: Disj[A]): Disj[A] = semigroupDisj.combine(x, y)
  }

  implicit final def semiringDisj[A](implicit ev: Semiring[A]): Semiring[Disj[A]] = new Semiring[Disj[A]] {
    override def zero: Disj[A] = Disj(ev.zero)

    override def times(x: Disj[A], y: Disj[A]): Disj[A] = Disj(ev.times(x.runDisj, y.runDisj))

    override def plus(x: Disj[A], y: Disj[A]): Disj[A] = Disj(ev.plus(x.runDisj, y.runDisj))
  }

  implicit final def functorDisj: Functor[Disj] = new Functor[Disj] {
    override def map[A, B](fa: Disj[A])(f: A => B): Disj[B] = Disj(f(fa.runDisj))
  }

  implicit final def applyDisj: Apply[Disj] = new Apply[Disj] {
    override def ap[A, B](ff: Disj[A => B])(fa: Disj[A]): Disj[B] = Disj(ff.runDisj(fa.runDisj))

    override def map[A, B](fa: Disj[A])(f: A => B): Disj[B] = functorDisj.fmap(fa)(f)
  }

  implicit final def applicativeDisj: Applicative[Disj] = new Applicative[Disj] {
    override def pure[A](x: A): Disj[A] = Disj(x)

    override def ap[A, B](ff: Disj[A => B])(fa: Disj[A]): Disj[B] = applyDisj.ap(ff)(fa)
  }

  implicit final def bindDisj: FlatMap[Disj] = new FlatMap[Disj] {
    override def flatMap[A, B](fa: Disj[A])(f: A => Disj[B]): Disj[B] = f(fa.runDisj)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Disj[Either[A, B]]): Disj[B] =
      f(a).runDisj match {
        case Right(value) => Disj(value)
        case Left(value)  => tailRecM(value)(f)
      }

    override def map[A, B](fa: Disj[A])(f: A => B): Disj[B] = functorDisj.fmap(fa)(f)
  }

  implicit final def monadDisj: Monad[Disj] = new Monad[Disj] {
    override def pure[A](x: A): Disj[A] = applicativeDisj.pure(x)

    override def flatMap[A, B](fa: Disj[A])(f: A => Disj[B]): Disj[B] = bindDisj.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Disj[scala.Either[A, B]]): Disj[B] = bindDisj.tailRecM(a)(f)
  }
}

object Disj extends DisjInstances
