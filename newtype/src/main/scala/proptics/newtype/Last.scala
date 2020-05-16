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

/** [[Semigroup]] where `combine` always takes the last option */
final case class Last[A](runLast: Option[A]) extends AnyVal

abstract class LastInstances {
  implicit final def eqLast[A: Eq]: Eq[Last[A]] = new Eq[Last[A]] {
    override def eqv(x: Last[A], y: Last[A]): Boolean =
      (x.runLast, y.runLast).mapN(_ === _).getOrElse(false)
  }

  implicit final def ordLast[A: Order]: Order[Last[A]] = new Order[Last[A]] {
    override def compare(x: Last[A], y: Last[A]): Int = x.runLast.compare(y.runLast)
  }

  implicit final def showLast[A: Show]: Show[Last[A]] = new Show[Last[A]] {
    override def show(t: Last[A]): String = s"(Last ${t.runLast.show})"
  }
  
  implicit final def semigroupLast[A]: Semigroup[Last[A]] = new Semigroup[Last[A]] {
    def combine(x: Last[A], y: Last[A]): Last[A] = Last(y.runLast orElse x.runLast)
  }

  implicit final def monoidLast[A]: Monoid[Last[A]] = new Monoid[Last[A]] {
    def empty: Last[A] = Last(None)

    def combine(x: Last[A], y: Last[A]): Last[A] = x |+| y
  }
  
  implicit final def functorLast: Functor[Last] = new Functor[Last] {
    override def map[A, B](fa: Last[A])(f: A => B): Last[B] = Last(fa.runLast.map(f))
  }

  implicit final def applyLast: Apply[Last] = new Apply[Last] {
    override def ap[A, B](ff: Last[A => B])(fa: Last[A]): Last[B] =
      Last((ff.runLast, fa.runLast).mapN(_(_)))

    override def map[A, B](fa: Last[A])(f: A => B): Last[B] = functorLast.fmap(fa)(f)
  }

  implicit final def applicativeLast: Applicative[Last] = new Applicative[Last] {
    override def pure[A](x: A): Last[A] = Last(x.some)

    override def ap[A, B](ff: Last[A => B])(fa: Last[A]): Last[B] = applyLast.ap(ff)(fa)
  }

  implicit final def bindLast: FlatMap[Last] = new FlatMap[Last] {
    override def flatMap[A, B](fa: Last[A])(f: A => Last[B]): Last[B] = Last(fa.runLast.flatMap(f(_).runLast))

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Last[Either[A, B]]): Last[B] =
      f(a).runLast match {
        case Some(Right(value)) => Last(value.some)
        case Some(Left(value))  => tailRecM(value)(f)
        case _                  => Last(None)
      }

    override def map[A, B](fa: Last[A])(f: A => B): Last[B] = functorLast.fmap(fa)(f)
  }

  implicit final def monadLast: Monad[Last] = new Monad[Last] {
    override def pure[A](x: A): Last[A] = applicativeLast.pure(x)

    override def flatMap[A, B](fa: Last[A])(f: A => Last[B]): Last[B] = bindLast.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Last[scala.Either[A, B]]): Last[B] = bindLast.tailRecM(a)(f)
  }
}

object Last extends LastInstances