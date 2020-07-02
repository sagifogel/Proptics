package proptics.law

import cats.laws._
import proptics.Lens

final case class LensLaws[S, A](lens: Lens[S, A]) extends AnyVal {
  private def getWhatYouSet(s: S): S = lens.set(lens.view(s))(s)
  private def setWhatYouGet(s: S)(a: A): A = lens.view(lens.set(a)(s))

  def getSet(s: S): IsEq[S] = getWhatYouSet(s) <-> s
  def setGet(s: S, a: A): IsEq[A] = setWhatYouGet(s)(a) <-> a
  def setSet(s: S, a: A): IsEq[S] = lens.set(a)(lens.set(a)(s)) <-> lens.set(a)(s)
  def overIdentity(s: S): IsEq[S] = lens.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = lens.over(g)(lens.over(f)(s)) <-> lens.over(g compose f)(s)
  def composeSourceIso(s: S): IsEq[S] = (getWhatYouSet _ compose getWhatYouSet)(s) <-> s
  def composeFocusIso(s: S, a: A): IsEq[A] = (setWhatYouGet(s) _ compose setWhatYouGet(s))(a) <-> a
}
