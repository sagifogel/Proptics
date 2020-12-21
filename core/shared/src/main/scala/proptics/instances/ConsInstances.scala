package proptics.instances

import proptics.{Cons, Prism}

trait ConsInstances {
  final def cons[S, A](implicit ev: Cons[S, A]): Prism[S, (A, S)] = ev.cons

  implicit final def consList[A]: Cons[List[A], A] = new Cons[List[A], A] {
    override def cons: Prism[List[A], (A, List[A])] =
      Prism.fromPartial[List[A], (A, List[A])] { case x :: xs => (x, xs) } { case (x, xs) => x :: xs }
  }
}
