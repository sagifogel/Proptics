package proptics.unsafe.instances

import proptics.Traversal
import proptics.typeclass.Each
import proptics.unsafe.{Traversal2, Traversal3, Traversal4, Traversal5}

trait UnsafeEachInstances {
  implicit def eachTuple2[A]: Each[(A, A), A] = new Each[(A, A), A] {
    override def each: Traversal[(A, A), A] =
      Traversal2[(A, A), (A, A), A, A](_._1, _._2)((b1, b2, _) => (b1, b2))
  }

  implicit def eachTuple3[A]: Each[(A, A, A), A] = new Each[(A, A, A), A] {
    override def each: Traversal[(A, A, A), A] =
      Traversal3[(A, A, A), (A, A, A), A, A](_._1, _._2, _._3)((b1, b2, b3, _) => (b1, b2, b3))
  }

  implicit def eachTuple4[A]: Each[(A, A, A, A), A] = new Each[(A, A, A, A), A] {
    override def each: Traversal[(A, A, A, A), A] =
      Traversal4[(A, A, A, A), (A, A, A, A), A, A](_._1, _._2, _._3, _._4)((b1, b2, b3, b4, _) => (b1, b2, b3, b4))
  }

  implicit def eachTuple5[A]: Each[(A, A, A, A, A), A] = new Each[(A, A, A, A, A), A] {
    override def each: Traversal[(A, A, A, A, A), A] =
      Traversal5[(A, A, A, A, A), (A, A, A, A, A), A, A](_._1, _._2, _._3, _._4, _._5)((b1, b2, b3, b4, b5, _) => (b1, b2, b3, b4, b5))
  }
}
