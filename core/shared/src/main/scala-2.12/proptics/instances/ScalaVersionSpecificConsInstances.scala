package proptics.instances

import proptics.{Cons, Prism}

private[instances] trait ScalaVersionSpecificConsInstances {
  implicit final def consStream[A]: Cons[Stream[A], A] = new Cons[Stream[A], A] {
    override def cons: Prism[Stream[A], (A, Stream[A])] =
      Prism.fromPreview[Stream[A], (A, Stream[A])](list => list.headOption.map((_, list.tail))) { case (head, tail) =>
        head #:: tail
      }
  }
}
