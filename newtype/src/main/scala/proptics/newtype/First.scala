package proptics.newtype

import cats.{Applicative, Apply, Eq, FlatMap, Functor, Monad, Order, Semigroup, Show}
import cats.kernel.Monoid
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.order._
import cats.syntax.semigroup._
import cats.syntax.option._
import cats.syntax.show._
import scala.annotation.tailrec

/** [[Semigroup]] where `combine` always takes the first option */
final case class First[A](runFirst: Option[A]) extends AnyVal

abstract class FirstInstances {
  implicit final def eqFirst[A: Eq]: Eq[First[A]] = new Eq[First[A]] {
    override def eqv(x: First[A], y: First[A]): Boolean =
      (x.runFirst, y.runFirst).mapN(_ === _).getOrElse(false)
  }

  implicit final def ordFirst[A: Order]: Order[First[A]] = new Order[First[A]] {
    override def compare(x: First[A], y: First[A]): Int = x.runFirst.compare(y.runFirst)
  }

  implicit final def showFirst[A: Show]: Show[First[A]] = new Show[First[A]] {
    override def show(t: First[A]): String = s"(First ${t.runFirst.show})"
  }

  implicit final def semigroupFirst[A]: Semigroup[First[A]] = new Semigroup[First[A]] {
    def combine(x: First[A], y: First[A]): First[A] = First(x.runFirst orElse y.runFirst)
  }

  implicit final def monoidFirst[A]: Monoid[First[A]] = new Monoid[First[A]] {
    def empty: First[A] = First(None)

    def combine(x: First[A], y: First[A]): First[A] = x |+| y
  }

  implicit final def functorFirst: Functor[First] = new Functor[First] {
    override def map[A, B](fa: First[A])(f: A => B): First[B] = First(fa.runFirst.map(f))
  }

  implicit final def applyFirst: Apply[First] = new Apply[First] {
    override def ap[A, B](ff: First[A => B])(fa: First[A]): First[B] =
      First((ff.runFirst, fa.runFirst).mapN(_(_)))

    override def map[A, B](fa: First[A])(f: A => B): First[B] = functorFirst.fmap(fa)(f)
  }

  implicit final def applicativeFirst: Applicative[First] = new Applicative[First] {
    override def pure[A](x: A): First[A] = First(x.some)

    override def ap[A, B](ff: First[A => B])(fa: First[A]): First[B] = applyFirst.ap(ff)(fa)
  }

  implicit final def bindFirst: FlatMap[First] = new FlatMap[First] {
    override def flatMap[A, B](fa: First[A])(f: A => First[B]): First[B] = First(fa.runFirst.flatMap(f(_).runFirst))

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => First[Either[A, B]]): First[B] =
      f(a).runFirst match {
        case Some(Right(value)) => First(value.some)
        case Some(Left(value))  => tailRecM(value)(f)
        case _                  => First(None)
      }

    override def map[A, B](fa: First[A])(f: A => B): First[B] = functorFirst.fmap(fa)(f)
  }

  implicit final def monadFirst: Monad[First] = new Monad[First] {
    override def pure[A](x: A): First[A] = applicativeFirst.pure(x)

    override def flatMap[A, B](fa: First[A])(f: A => First[B]): First[B] = bindFirst.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => First[scala.Either[A, B]]): First[B] = bindFirst.tailRecM(a)(f)
  }
}

object First extends FirstInstances
