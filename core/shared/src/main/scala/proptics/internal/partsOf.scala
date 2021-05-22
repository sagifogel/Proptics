package proptics.internal

import cats.data.State

import proptics.Traversal_

private[proptics] object partsOf {
  def ins[A, T](bazaar: Bazaar[* => *, A, A, Unit, T]): List[A] =
    Traversal_.fromBazaar[Unit, T, A, A](bazaar).toList(())

  def outs[A, T](bazaar: Bazaar[* => *, A, A, Unit, T]): List[A] => T = list =>
    bazaar.runBazaar
      .apply(a => State(unconsWithDefault(a)))(())
      .runA(list)
      .value

  def unconsWithDefault[A](a: A): List[A] => (List[A], A) = {
    case Nil => (Nil, a)
    case x :: xs => (xs, x)
  }
}
