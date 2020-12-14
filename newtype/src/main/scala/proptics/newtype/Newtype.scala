package proptics.newtype

import cats.{Functor, Id}

import proptics.newtype.Newtype.Aux

/** type class for newtypes to enable convenient wrapping and unwrapping a value */
trait Newtype[T] {
  type A

  def wrap(value: A): T

  def unwrap(value: T): A
}

abstract class NewtypeInstances {
  implicit final def newtypeId[A0]: Aux[Id[A0], A0] = new Newtype[Id[A0]] {
    override type A = A0

    override def wrap(value: this.A): Id[A] = value

    override def unwrap(value: Id[A]): this.A = value
  }

  implicit final def newtypeAdditive[A0]: Aux[Additive[A0], A0] = new Newtype[Additive[A0]] {
    override type A = A0

    override def wrap(value: this.A): Additive[A] = Additive(value)

    override def unwrap(value: Additive[A]): this.A = value.runAdditive
  }

  implicit final def newtypeMultiplicative[A0]: Aux[Multiplicative[A0], A0] = new Newtype[Multiplicative[A0]] {
    override type A = A0

    override def wrap(value: this.A): Multiplicative[A] = Multiplicative(value)

    override def unwrap(value: Multiplicative[A]): this.A = value.runMultiplicative
  }

  implicit final def newtypeConj[A0]: Aux[Conj[A0], A0] = new Newtype[Conj[A0]] {
    override type A = A0

    override def wrap(value: this.A): Conj[A] = Conj(value)

    override def unwrap(value: Conj[A]): this.A = value.runConj
  }

  implicit final def newtypeDisj[A0]: Aux[Disj[A0], A0] = new Newtype[Disj[A0]] {
    override type A = A0

    override def wrap(value: this.A): Disj[A] = Disj(value)

    override def unwrap(value: Disj[A]): this.A = value.runDisj
  }

  implicit final def newtypeDual[A0]: Aux[Dual[A0], A0] = new Newtype[Dual[A0]] {
    override type A = A0

    override def wrap(value: this.A): Dual[A] = Dual(value)

    override def unwrap(value: Dual[A]): this.A = value.runDual
  }

  implicit final def newtypeEndo[C[_, _], A0]: Aux[Endo[C, A0], C[A0, A0]] = new Newtype[Endo[C, A0]] {
    override type A = C[A0, A0]

    override def wrap(value: this.A): Endo[C, A0] = Endo(value)

    override def unwrap(value: Endo[C, A0]): this.A = value.runEndo
  }

  implicit final def newtypeFirst[A0]: Aux[First[A0], Option[A0]] = new Newtype[First[A0]] {
    override type A = Option[A0]

    override def wrap(value: this.A): First[A0] = First(value)

    override def unwrap(value: First[A0]): this.A = value.runFirst
  }

  implicit final def newtypeLast[A0]: Aux[Last[A0], Option[A0]] = new Newtype[Last[A0]] {
    override type A = Option[A0]

    override def wrap(value: this.A): Last[A0] = Last(value)

    override def unwrap(value: Last[A0]): this.A = value.runLast
  }
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

  def ala[F[_], T, A, S, B](f: (B => S) => F[T])(implicit ev0: Functor[F], ev1: Newtype.Aux[T, A], ev2: Newtype.Aux[S, B]): F[A] =
    ev0.map(f(ev2.wrap))(ev1.unwrap)

  def alaF[F[_], G[_], T, A, S, B](f: F[T] => G[S])(fa: F[A])(implicit ev0: Functor[F], ev1: Functor[G], ev2: Newtype.Aux[T, A], ev3: Newtype.Aux[S, B]): G[B] =
    ev1.map(f(ev0.map(fa)(ev2.wrap)))(ev3.unwrap)

  def over[T, A, S, B](f: A => B)(t: T)(implicit ev0: Newtype.Aux[T, A], ev1: Newtype.Aux[S, B]): S =
    overF[Id, Id, T, A, S, B](f)(t)

  def overF[F[_], G[_], T, A, S, B](f: F[A] => G[B])(ft: F[T])(implicit ev0: Functor[F], ev1: Functor[G], ev2: Newtype.Aux[T, A], ev3: Newtype.Aux[S, B]): G[S] =
    ev1.map(f(ev0.map(ft)(ev2.unwrap)))(ev3.wrap)

  def under[T, A, S, B](f: T => S)(a: A)(implicit ev0: Newtype.Aux[T, A], ev1: Newtype.Aux[S, B]): B =
    underF[Id, Id, T, A, S, B](f)(a)

  def underF[F[_], G[_], T, A, S, B](f: F[T] => G[S])(fa: F[A])(implicit ev0: Functor[F], ev1: Functor[G], ev2: Newtype.Aux[T, A], ev3: Newtype.Aux[S, B]): G[B] =
    ev1.map(f(ev0.map(fa)(ev2.wrap)))(ev3.unwrap)

  private def on[A, B, C](f: B => B => C)(g: A => B): A => A => C = x => y => f(g(x))(g(y))

  def over2[T, A, S, B](g: A => T)(f: A => A => B)(implicit ev0: Newtype.Aux[T, A], ev1: Newtype.Aux[S, B]): T => T => S =
    overF2[Id, Id, T, A, S, B](f)

  def overF2[F[_], G[_], T, A, S, B](f: F[A] => F[A] => G[B])(implicit ev1: Functor[F], ev2: Functor[G], ev3: Newtype.Aux[T, A], ev4: Newtype.Aux[S, B]): F[T] => F[T] => G[S] =
    ev2.lift(ev4.wrap) compose on(f)(ev1.lift(ev3.unwrap))(_)

  def under2[T, A, S, B](f: T => T => S)(implicit ev0: Newtype.Aux[T, A], ev1: Newtype.Aux[S, B]): A => A => B =
    underF2[Id, Id, T, A, S, B](f)

  def underF2[F[_], G[_], T, A, S, B](f: F[T] => F[T] => G[S])(implicit ev0: Functor[F], ev1: Functor[G], ev2: Newtype.Aux[T, A], ev3: Newtype.Aux[S, B]): F[A] => F[A] => G[B] =
    ev1.lift(ev3.unwrap) compose on(f)(ev0.lift(ev2.wrap))(_)

  def traverse[F[_], T, A, S, B](t: T)(f: A => F[A])(implicit ev0: Functor[F], ev1: Newtype.Aux[T, A]): T => F[T] =
    ev0.lift(ev1.wrap) compose f compose ev1.unwrap

  def collect[F[_], T, A, S, B](f: F[A] => A)(implicit ev0: Functor[F], ev1: Newtype.Aux[T, A]): F[T] => T =
    ev1.wrap _ compose f compose ev0.lift(ev1.unwrap)
}
