package optics

import cats.{Applicative, Id}
import optics.internal.{Traversing, Wander}

trait Index[P[_, _], A, B, C] {
  def ix(f: A): Traversal_[P, A, C]
}

abstract class IndexInstances {
  implicit final def indexIdentity[P[_, _], A](implicit ev: Wander[P]): Index[P, Id[A], Unit, A] = new Index[P, Id[A], Unit, A] {
    override def ix(f: Id[A]): Traversal_[P, Id[A], A] = new Traversal_[P, Id[A], A] {
      override def apply(pab: P[A, A]): P[Id[A], Id[A]] = {
        val traversing: Traversing[Id[A], Id[A], A, A] = new Traversing[Id[A], Id[A], A, A] {
          override def apply[F[_]](f: A => F[A])(implicit ev: Applicative[F]): Id[A] => F[Id[A]] = f
        }

        ev.wander(traversing)(pab)
      }
    }
  }
}

object Index extends IndexInstances
