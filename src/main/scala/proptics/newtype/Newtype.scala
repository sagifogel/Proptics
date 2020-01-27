package proptics.newtype

import cats.Functor
import proptics.syntax.FunctionSyntax._

trait Newtype[T] {
  type A
  def wrap(value: A): T
  def unwrap(value: T): A
}

abstract class NewtypeInstances {
}

object Newtype extends NewtypeInstances {
  type Aux[T, A0] = Newtype[T] { type A = A0 }

  def apply[T](implicit ev: Newtype[T]): Aux[T, ev.A] = ev

  def newtype[T, A0](f: A0 => T)(g: T => A0): Aux[T, A0] = new Newtype[T] {
    type A = A0

    def wrap(value: A): T = f(value)

    def unwrap(value: T): A = g(value)
  }

  def un[T, A](f: A => T)(t: T)(implicit ev: Newtype.Aux[T, A]): A = ev.unwrap(t)

  def op[T, A](f: A => T)(t: T)(implicit ev: Newtype.Aux[T, A]): A = un(f)(t)

  def ala[F[_], T, A, S, B](g: A => T)
                           (f: (B => S) => F[T])
                           (implicit ev: Functor[F],
                            ev2: Newtype.Aux[T, A],
                            ev3: Newtype.Aux[S, B]): F[A] = ev.map(f(ev3.wrap))(ev2.unwrap)

  def alaF[F[_], G[_], T, A, S, B](g: A => T)
                                  (f: F[T] => G[S])
                                  (implicit ev1: Functor[F],
                                   ev2: Functor[G],
                                   ev3: Newtype.Aux[T, A],
                                   ev4: Newtype.Aux[S, B]): F[A] => G[B] = {
    ev2.lift(ev4.unwrap) compose f compose ev1.lift(ev3.wrap)
  }

  def over[T, A, S, B](g: A => T)
                      (f: A => B)
                      (implicit ev: Newtype.Aux[T, A],
                       ev2: Newtype.Aux[S, B]): T => S = ev2.wrap _ compose f compose ev.unwrap

  def overF[F[_], G[_], T, A, S, B](g: A => T)
                                   (f: F[A] => G[B])
                                   (implicit ev1: Functor[F],
                                    ev2: Functor[G],
                                    ev3: Newtype.Aux[T, A],
                                    ev4: Newtype.Aux[S, B]): F[T] => G[S] =
    ev2.lift(ev4.wrap) compose f compose ev1.lift(ev3.unwrap)

  def under[T, A, S, B](g: A => T)
                       (f: T => S)
                       (implicit ev: Newtype.Aux[T, A],
                        ev2: Newtype.Aux[S, B]): A => B = ev2.unwrap _ compose f compose ev.wrap

  def underF[F[_], G[_], T, A, S, B](g: A => T)
                                    (f: F[T] => G[S])
                                    (implicit ev1: Functor[F],
                                     ev2: Functor[G],
                                     ev3: Newtype.Aux[T, A],
                                     ev4: Newtype.Aux[S, B]): F[A] => G[B] =
    ev2.lift(ev4.unwrap) compose f compose ev1.lift(ev3.wrap)

  def over2[T, A, S, B](g: A => T)
                       (f: A => A => B)
                       (implicit ev: Newtype.Aux[T, A],
                        ev2: Newtype.Aux[S, B]): T => T => S =
    ev2.wrap _ compose f.on(ev.unwrap)(_)

  def overF2[F[_], G[_], T, A, S, B](g: A => T)
                                    (f: F[A] => F[A] => G[B])
                                    (implicit ev1: Functor[F],
                                     ev2: Functor[G],
                                     ev3: Newtype.Aux[T, A],
                                     ev4: Newtype.Aux[S, B]): F[T] => F[T] => G[S] =
    ev2.lift(ev4.wrap) compose f.on(ev1.lift(ev3.unwrap))(_)

  def under2[T, A, S, B](g: A => T)
                        (f: T => T => S)
                        (implicit ev1: Newtype.Aux[T, A],
                         ev2: Newtype.Aux[S, B]): A => A => B =
    ev2.unwrap _ compose f.on(ev1.wrap)(_)

  def underF2[F[_], G[_], T, A, S, B](g: A => T)
                                     (f: F[T] => F[T] => G[S])
                                     (implicit ev1: Functor[F],
                                      ev2: Functor[G],
                                      ev3: Newtype.Aux[T, A],
                                      ev4: Newtype.Aux[S, B]): F[A] => F[A] => G[B] =
    ev2.lift(ev4.unwrap) compose f.on(ev1.lift(ev3.wrap))(_)

  def traverse[F[_], T, A, S, B](g: A => T)
                                (f: A => F[A])
                                (implicit ev1: Functor[F],
                                 ev2: Newtype.Aux[T, A]): T => F[T] =
    ev1.lift(ev2.wrap) compose f compose ev2.unwrap

  def collect[F[_], T, A, S, B](g: A => T)
                               (f: F[A] => A)
                               (implicit ev1: Functor[F],
                                ev2: Newtype.Aux[T, A]): F[T] => T =
    ev2.wrap _ compose f compose ev1.lift(ev2.unwrap)
}
