package proptics.instances

import proptics.Traversal
import proptics.typeclass.Each

private[instances] trait ScalaVersionSpecificEachInstances {
  implicit final def eachStream[A]: Each[Stream[A], A] = new Each[Stream[A], A] {
    override def each: Traversal[Stream[A], A] = Traversal.fromTraverse[Stream, A]
  }
}
