package proptics.law

import cats.laws._

import proptics.ALens

trait ALensLaws[S, A] {
  def aLens: ALens[S, A]

  private def setWhatYouGet(s: S): S = aLens.set(aLens.view(s))(s)
  private def getWhatYouSet(s: S)(a: A): A = aLens.view(aLens.set(a)(s))

  def setGet(s: S): IsEq[S] = setWhatYouGet(s) <-> s
  def getSet(s: S, a: A): IsEq[A] = getWhatYouSet(s)(a) <-> a
  def setSet(s: S, a: A): IsEq[S] = aLens.set(a)(aLens.set(a)(s)) <-> aLens.set(a)(s)
  def overIdentity(s: S): IsEq[S] = aLens.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = aLens.over(g)(aLens.over(f)(s)) <-> aLens.over(g compose f)(s)
  def composeSourceLens(s: S): IsEq[S] = setWhatYouGet _ compose setWhatYouGet (s) <-> s
  def composeFocusLens(s: S, a: A): IsEq[A] = getWhatYouSet(s) _ compose getWhatYouSet(s) (a) <-> a
}

object ALensLaws {
  def apply[S, A](_aLens: ALens[S, A]): ALensLaws[S, A] =
    new ALensLaws[S, A] { override def aLens: ALens[S, A] = _aLens }
}
