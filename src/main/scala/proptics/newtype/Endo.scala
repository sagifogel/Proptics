package proptics.newtype

import cats.kernel.Monoid

final case class Endo[C[_, _], A](runEndo: C[A, A]) extends AnyVal

abstract class EndoInstances {
  implicit final def monoidEndo[A]: Monoid[Endo[* => *, A]] = new Monoid[Endo[* => *, A]] {
    override def empty: Endo[* => *, A] = Endo(identity)

    override def combine(x: Endo[* => *, A], y: Endo[* => *, A]): Endo[* => *, A] =
      Endo(x.runEndo compose y.runEndo)
  }
}

object Endo extends EndoInstances