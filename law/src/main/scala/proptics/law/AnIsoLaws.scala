package proptics.law

import cats.laws._
import proptics.AnIso

trait AnIsoLaws[S, A] {
  def anIso: AnIso[S, A]

  private def sourceBackAndForth: S => S = anIso.review _ compose anIso.view
  private def focusBackAndForth: A => A = anIso.view _ compose anIso.review

  def sourceReversibility(s: S): IsEq[S] = sourceBackAndForth(s) <-> s
  def focusReversibility(a: A): IsEq[A] = focusBackAndForth(a) <-> a
  def overIdentity(s: S): IsEq[S] = anIso.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = anIso.over(g)(anIso.over(f)(s)) <-> anIso.over(g compose f)(s)
  def composeSourceIso(s: S): IsEq[S] = (sourceBackAndForth compose sourceBackAndForth)(s) <-> s
  def composeFocusIso(a: A): IsEq[A] = (focusBackAndForth compose focusBackAndForth)(a) <-> a
}

object AnIsoLaws {
  def apply[S, A](_anIso: AnIso[S, A]): AnIsoLaws[S, A] =
    new AnIsoLaws[S, A] { def anIso: AnIso[S, A] = _anIso }
}
