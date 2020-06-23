package proptics.specs

import cats.laws._
import proptics.Iso

final case class IsoLaws[S, A](iso: Iso[S, A]) extends AnyVal {
  private def sourceBackAndForth: S => S = iso.review _ compose iso.view
  private def focusBackAndForth: A => A = iso.view _ compose iso.review

  def sourceReversibility(s: S): IsEq[S] = sourceBackAndForth(s) <-> s
  def focusReversibility(a: A): IsEq[A] = focusBackAndForth(a) <-> a
  def overIdentity(s: S): IsEq[S] = iso.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = iso.over(g)(iso.over(f)(s)) <-> iso.over(g compose f)(s)
  def composeSourceIso(s: S): IsEq[S] = (sourceBackAndForth compose sourceBackAndForth)(s) <-> s
  def composeFocusIso(a: A): IsEq[A] = (focusBackAndForth compose focusBackAndForth)(a) <-> a
}
