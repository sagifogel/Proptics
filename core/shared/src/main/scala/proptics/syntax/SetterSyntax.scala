package proptics.syntax

import proptics.Setter_

trait SetterSyntax extends SetterSyntaxCompat {
  implicit def setterSTAOptionB[S, T, A, B](setter: Setter_[S, T, A, Option[B]]): SetterSTAOptionB[S, T, A, B] = SetterSTAOptionB(setter)
}

final case class SetterSTAOptionB[S, T, A, B](private val setter: Setter_[S, T, A, Option[B]]) extends AnyVal {
  /** modify the focus type of a [[Setter_]] using an Option */
  def setJust(s: S)(b: B): T = setter.set(Some(b))(s)
}
