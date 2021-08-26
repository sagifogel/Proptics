package proptics.data

import cats.arrow.{Category, Compose}
import cats.syntax.compose._
import cats.syntax.order._
import cats.{Eq, Monoid, Order, Semigroup}

/** [[cats.Monoid]] and [[cats.Semigroup]] for category endomorphisms.
  * <p>
  * When `c` is instantiated with `->` this composes functions of type `a -> a`
  * </p>
  */
final case class Endo[C[_, _], A](runEndo: C[A, A])

abstract class EndoInstances {
  implicit final def eqEndo[C[_, _], A](implicit ev: Eq[C[A, A]]): Eq[Endo[C, A]] = new Eq[Endo[C, A]] {
    override def eqv(x: Endo[C, A], y: Endo[C, A]): Boolean = x.runEndo === y.runEndo
  }

  implicit final def ordEndo[C[_, _], A](implicit ev: Order[C[A, A]]): Order[Endo[C, A]] = new Order[Endo[C, A]] {
    override def compare(x: Endo[C, A], y: Endo[C, A]): Int = x.runEndo.compare(y.runEndo)
  }

  implicit final def semigroupEndo[C[_, _], A](implicit ev: Compose[C]): Semigroup[Endo[C, A]] = new Semigroup[Endo[C, A]] {
    override def combine(x: Endo[C, A], y: Endo[C, A]): Endo[C, A] =
      Endo(x.runEndo <<< y.runEndo)
  }

  implicit final def monoidEndo[C[_, _], A](implicit ev: Category[C]): Monoid[Endo[C, A]] = new Monoid[Endo[C, A]] {
    override def empty: Endo[C, A] = Endo(ev.id)

    override def combine(x: Endo[C, A], y: Endo[C, A]): Endo[C, A] = semigroupEndo[C, A].combine(x, y)
  }
}

object Endo extends EndoInstances
