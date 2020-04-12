package proptics

import cats.Eq
import cats.Monoid
import cats.syntax.eq._
import cats.syntax.option._
import proptics.internal.Forget
import proptics.newtype.Disj
import spire.algebra.lattice.Heyting


/**
  * A [[Getter_]] is a [[Fold]] that ignores the supplied Monoid
  *
  * @tparam S the source of a [[Getter_]]
  * @tparam T the modified source of a [[Getter_]]
  * @tparam A the target of a [[Getter_]]
  * @tparam B the modified target of a [[Getter_]]
  */
abstract class Getter_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T]

  def view(s: S): A = self(Forget(identity)).runForget(s)

  def exists(f: A => Boolean): S => Boolean = f compose view

  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  def has(s: S)(implicit ev: Heyting[A]): A = hasOrHasnt(s)(ev.one)

  def hasNot(s: S)(implicit ev: Heyting[A]): A = hasOrHasnt(s)(ev.zero)

  def find(f: A => Boolean): S => Option[A] = s => view(s).some.find(f)

  def asFold_ : Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(s => forget.runForget(self(Forget(identity)).runForget(s)))
  }

  private[proptics] def hasOrHasnt(s: S)(r: A)(implicit ev: Heyting[A]): A =
    Monoid[Disj[A]].combine(Disj(view(s)), Disj(ev.one)).runDisj
}

object Getter_ {
  private[Getter_] def apply[S, T, A, B](aGetter: Forget[A, A, B] => Forget[A, S, T]): Getter_[S, T, A, B] = new Getter_[S, T, A, B] {
    override def apply(forget: Forget[A, A, B]): Forget[A, S, T] = aGetter(forget)
  }

  def apply[S, T, A, B](f: S => A)(implicit ev: DummyImplicit): Getter_[S, T, A, B] =
    Getter_((forget: Forget[A, A, B]) => Forget[A, S, T](forget.runForget compose f))

}

object Getter {
  def apply[S, A](f: S => A): Getter[S, A] = Getter_(f)
}
