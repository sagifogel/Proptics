package optics

import cats.Id
import optics.internal.Wander

trait Index[P[_, _], A, B, C] {
  def ix(f: A): Traversal_[P, A, B]
}

abstract class IndexInstances {
  implicit final def indexIdentity[P[_, _]: Wander, A]: Index[P, Id[A], Unit, A] = new Index[P, Id[A], Unit, A] {
    override def ix(f: Id[A]): Traversal_[P, Id[A], Unit] = ???
  }
}

object Index extends IndexInstances
