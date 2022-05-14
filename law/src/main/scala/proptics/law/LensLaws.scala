package proptics.law

import cats.laws._

import proptics.Lens

trait LensLaws[S, A] {
  def lens: Lens[S, A]
  private def setWhatYouGet(s: S): S = lens.set(lens.view(s))(s)
  private def getWhatYouSet(s: S)(a: A): A = lens.view(lens.set(a)(s))

  def setGet(s: S): IsEq[S] = setWhatYouGet(s) <-> s
  def getSet(s: S, a: A): IsEq[A] = getWhatYouSet(s)(a) <-> a
  def setSet(s: S, a: A): IsEq[S] = lens.set(a)(lens.set(a)(s)) <-> lens.set(a)(s)
  def overIdentity(s: S): IsEq[S] = lens.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = lens.over(g)(lens.over(f)(s)) <-> lens.over(g compose f)(s)
  def composeSourceLens(s: S): IsEq[S] = setWhatYouGet _ compose setWhatYouGet (s) <-> s
  def composeFocusLens(s: S, a: A): IsEq[A] = getWhatYouSet(s) _ compose getWhatYouSet(s) (a) <-> a
}

object LensLaws {
  def apply[S, A](_lens: Lens[S, A]): LensLaws[S, A] =
    new LensLaws[S, A] { override def lens: Lens[S, A] = _lens }
}
