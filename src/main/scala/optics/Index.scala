package optics

import cats.{Applicative, Eq, Id}
import cats.syntax.eq._
import optics.internal.{Traversing, Wander}

import scala.Function.const

trait Index[P[_, _], M, A, B] {
  def ix(a: A): Traversal_[P, M, B]
}

abstract class IndexInstances {
  implicit final def indexArr[P[_, _], I, A](implicit ev: Wander[P], ev2: Eq[I]): Index[P, I => A, I, A] = new Index[P, I => A, I, A] {
    override def ix(i: I): Traversal_[P, I => A, A] = new Traversal_[P, I => A, A] {
      override def apply(pab: P[A, A]): P[I => A, I => A] = {
        val traversing: Traversing[I => A, I => A, A, A] = new Traversing[I => A, I => A, A, A] {
          override def apply[F[_]](coalg: A => F[A])(implicit ev: Applicative[F]): (I => A) => F[I => A] = f =>
            ev.map(coalg(f(i)))(a => j => if (i === j) a else f(j))
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexOption[P[_, _], A](implicit ev: Wander[P]): Index[P, Option[A], Unit, A] = new Index[P, Option[A], Unit, A] {
    override def ix(f: Unit): Traversal_[P, Option[A], A] = new Traversal_[P, Option[A], A] {
      override def apply(pab: P[A, A]): P[Option[A], Option[A]] = {
        val traversing: Traversing[Option[A], Option[A], A, A] = new Traversing[Option[A], Option[A], A, A] {
          override def apply[F[_]](f: A => F[A])(implicit ev: Applicative[F]): Option[A] => F[Option[A]] = ev.pure
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexIdentity[P[_, _], A](implicit ev: Wander[P]): Index[P, Id[A], Unit, A] = new Index[P, Id[A], Unit, A] {
    override def ix(a: Unit): Traversal_[P, Id[A], A] = Traversal_[P, Id[A], A](identity)(const(identity))
  }
}

object Index extends IndexInstances
