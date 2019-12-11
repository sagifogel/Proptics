package optics.newtype

import cats.{Functor, Id}

trait Newtype[T, A] {
  def wrap(a: A): T
  def unwrap(t: T): A
}

abstract class NewtypeInstances {
  implicit def newtypeIdentity[A]: Newtype[Id[A], A] = new Newtype[Id[A], A] {
    override def wrap(a: A): Id[A] = a

    override def unwrap(t: Id[A]): A = t
  }
}

object Newtype extends NewtypeInstances {
  def alaF[F[_], G[_], T, A, S, B](g: A => T)
                                  (f: F[T] => G[S])
                                  (fa : F[A])
                                  (implicit ev1: Functor[F],
                                   ev2: Functor[G],
                                   ev3: Newtype[T, A],
                                   ev4: Newtype[S, B]): G[B] = {
      val ft: F[T] = ev1.map(fa)(ev3.wrap)
      val gs: G[S] = f(ft)

      ev2.map(gs)(ev4.unwrap)
  }
}
