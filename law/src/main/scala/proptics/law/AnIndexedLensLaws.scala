package proptics.law

import cats.laws._
import proptics.AnIndexedLens

case class AnIndexedLensLaws[I, S, A](anIndexedLens: AnIndexedLens[I, S, A]) extends AnyVal {
  private def setWhatYouGet(s: S): S = anIndexedLens.set(anIndexedLens.view(s)._2)(s)
  private def getWhatYouSet(s: S)(a: A): A = anIndexedLens.view(anIndexedLens.set(a)(s))._2

  def setGet(s: S): IsEq[S] = setWhatYouGet(s) <-> s
  def getSet(s: S, a: A): IsEq[A] = getWhatYouSet(s)(a) <-> a
  def setSet(s: S, a: A): IsEq[S] = anIndexedLens.set(a)(anIndexedLens.set(a)(s)) <-> anIndexedLens.set(a)(s)
  def overIdentity(s: S): IsEq[S] = anIndexedLens.over(_._2)(s) <-> s
  def composeOver(s: S)(f: (I, A) => A)(g: (I, A) => A): IsEq[S] =
    anIndexedLens.over(g.tupled)(anIndexedLens.over(f.tupled)(s)) <->
      anIndexedLens.over { case (i, a) => g(i, f(i, a)) }(s)

  def composeSourceLens(s: S): IsEq[S] = (setWhatYouGet _ compose setWhatYouGet)(s) <-> s
  def composeFocusLens(s: S, a: A): IsEq[A] = (getWhatYouSet(s) _ compose getWhatYouSet(s))(a) <-> a
}
