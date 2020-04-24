package proptics

import cats.{Eq, Monoid}
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
      Forget(forget.runForget compose self.view)
  }

  def compose[C, D](other: Iso_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): Getter_[S, T, C, D] = self compose other.asIso_

  def compose[C, D](other: Lens_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  def compose[C, D](other: ALens_[A, B, C, D]): Getter_[S, T, C, D] = self compose other.asLens_

  def compose[C, D](other: Prism_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.preview(self.view(s)).fold(Monoid[R].empty)(forget.runForget))
  }

  def compose[C, D](other: APrism_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asPrism_

  def compose[C, D](other: Traversal_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(forget.runForget))
  }

  def compose[C, D](other: ATraversal_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asTraversal_

  def compose[C, D](other: Getter_[A, B, C, D]): Getter_[S, T, C, D] = new Getter_[S, T, C, D] {
    override private[proptics] def apply(forget: Forget[C, C, D]): Forget[C, S, T] =
      Forget(forget.runForget compose other.view compose self.view)
  }

  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s))(forget.runForget))
  }

  private[proptics] def hasOrHasnt(s: S)(r: A)(implicit ev: Heyting[A]): A =
    Monoid[Disj[A]].combine(Disj(view(s)), Disj(ev.one)).runDisj
}

object Getter_ {
  private[Getter_] def apply[S, T, A, B](f: Forget[A, A, B] => Forget[A, S, T]): Getter_[S, T, A, B] = new Getter_[S, T, A, B] {
    override def apply(forget: Forget[A, A, B]): Forget[A, S, T] = f(forget)
  }

  def apply[S, T, A, B](f: S => A)(implicit ev: DummyImplicit): Getter_[S, T, A, B] =
    Getter_((forget: Forget[A, A, B]) => Forget[A, S, T](forget.runForget compose f))

}

object Getter {
  def apply[S, A](f: S => A): Getter[S, A] = Getter_(f)
}
