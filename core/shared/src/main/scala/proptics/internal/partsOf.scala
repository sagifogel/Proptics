package proptics.internal

import cats.data.State

import proptics.Traversal_
import proptics.profunctor.Corepresentable.Aux

private[proptics] object partsOf {
  def ins[A, B, T](bazaar: Bazaar[* => *, A, B, Unit, T]): List[A] =
    Traversal_.fromBazaar[Unit, T, A, B](bazaar).toList(())

  def outs[A, T](bazaar: Bazaar[* => *, A, A, Unit, T]): List[A] => T = list =>
    bazaar
      .runBazaar(a => State(unconsWithDefault(a)))(())
      .runA(list)
      .value

  def unsafeOuts[A, B, T](bazaar: Bazaar[* => *, A, B, Unit, T])(implicit ev: Aux[* => *, State[List[B], *]]): List[B] => T = list =>
    bazaar
      .runBazaar(ev.cotabulate[A, State[List[B], B]](_ => State(unconsWithDefault(fakeVal))))(())
      .runA(list)
      .value

  def unconsWithDefault[A](a: => A): List[A] => (List[A], A) = {
    case Nil => (Nil, a)
    case x :: xs => (xs, x)
  }

  def fakeVal[B]: B =
    throw new IllegalArgumentException("Not enough elements were supplied")
}
