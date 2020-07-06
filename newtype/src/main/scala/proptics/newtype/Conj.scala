package proptics.newtype

import cats.syntax.order._
import cats.syntax.show._
import cats.{Applicative, Apply, Eq, FlatMap, Functor, Monad, Monoid, Order, Semigroup, Show}
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting

import scala.annotation.tailrec

/** [[Monoid]] and [[Semigroup]] for conjunction */
final case class Conj[A](runConj: A) extends AnyVal

abstract class ConjInstances {
  implicit final def eqConj[A: Eq]: Eq[Conj[A]] = new Eq[Conj[A]] {
    override def eqv(x: Conj[A], y: Conj[A]): Boolean = x.runConj === y.runConj
  }

  implicit final def ordConj[A: Order]: Order[Conj[A]] = new Order[Conj[A]] {
    override def compare(x: Conj[A], y: Conj[A]): Int = x.runConj.compare(y.runConj)
  }

  implicit final def showConj[A: Show]: Show[Conj[A]] = new Show[Conj[A]] {
    override def show(t: Conj[A]): String = s"(Conj ${t.runConj.show})"
  }

  implicit final def semigroupConj[A](implicit ev: Heyting[A]): Semigroup[Conj[A]] = new Semigroup[Conj[A]] {
    override def combine(x: Conj[A], y: Conj[A]): Conj[A] = Conj(ev.and(x.runConj, y.runConj))
  }

  implicit final def monoidConj[A](implicit ev: Heyting[A]): Monoid[Conj[A]] = new Monoid[Conj[A]] {
    override def empty: Conj[A] = Conj(ev.one)

    override def combine(x: Conj[A], y: Conj[A]): Conj[A] = semigroupConj.combine(x, y)
  }

  implicit final def semiringConj[A](implicit ev: Semiring[A]): Semiring[Conj[A]] = new Semiring[Conj[A]] {
    override def zero: Conj[A] = Conj(ev.zero)

    override def times(x: Conj[A], y: Conj[A]): Conj[A] = Conj(ev.times(x.runConj, y.runConj))

    override def plus(x: Conj[A], y: Conj[A]): Conj[A] = Conj(ev.plus(x.runConj, y.runConj))
  }

  implicit final def functorConj: Functor[Conj] = new Functor[Conj] {
    override def map[A, B](fa: Conj[A])(f: A => B): Conj[B] = Conj(f(fa.runConj))
  }

  implicit final def applyConj: Apply[Conj] = new Apply[Conj] {
    override def ap[A, B](ff: Conj[A => B])(fa: Conj[A]): Conj[B] = Conj(ff.runConj(fa.runConj))

    override def map[A, B](fa: Conj[A])(f: A => B): Conj[B] = functorConj.fmap(fa)(f)
  }

  implicit final def applicativeConj: Applicative[Conj] = new Applicative[Conj] {
    override def pure[A](x: A): Conj[A] = Conj(x)

    override def ap[A, B](ff: Conj[A => B])(fa: Conj[A]): Conj[B] = applyConj.ap(ff)(fa)
  }

  implicit final def bindConj: FlatMap[Conj] = new FlatMap[Conj] {
    override def flatMap[A, B](fa: Conj[A])(f: A => Conj[B]): Conj[B] = f(fa.runConj)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Conj[Either[A, B]]): Conj[B] =
      f(a).runConj match {
        case Right(value) => Conj(value)
        case Left(value)  => tailRecM(value)(f)
      }

    override def map[A, B](fa: Conj[A])(f: A => B): Conj[B] = functorConj.fmap(fa)(f)
  }

  implicit final def monadConj: Monad[Conj] = new Monad[Conj] {
    override def pure[A](x: A): Conj[A] = applicativeConj.pure(x)

    override def flatMap[A, B](fa: Conj[A])(f: A => Conj[B]): Conj[B] = bindConj.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Conj[scala.Either[A, B]]): Conj[B] = bindConj.tailRecM(a)(f)
  }
}

object Conj extends ConjInstances
