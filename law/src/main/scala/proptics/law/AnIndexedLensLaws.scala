package proptics.law

import cats.laws._

import proptics.AnIndexedLens

trait AnIndexedLensLaws[I, S, A] {
  def anIndexedLens: AnIndexedLens[I, S, A]

  private def setWhatYouGet(s: S): S = anIndexedLens.set(anIndexedLens.view(s)._1)(s)
  private def getWhatYouSet(s: S)(a: A): A = anIndexedLens.view(anIndexedLens.set(a)(s))._1

  def setGet(s: S): IsEq[S] = setWhatYouGet(s) <-> s
  def getSet(s: S, a: A): IsEq[A] = getWhatYouSet(s)(a) <-> a
  def setSet(s: S, a: A): IsEq[S] = anIndexedLens.set(a)(anIndexedLens.set(a)(s)) <-> anIndexedLens.set(a)(s)
  def overIdentity(s: S): IsEq[S] = anIndexedLens.over(_._1)(s) <-> s
  def composeOver(s: S)(f: (A, I) => A)(g: (A, I) => A): IsEq[S] =
    anIndexedLens.over(g.tupled)(anIndexedLens.over(f.tupled)(s)) <->
      anIndexedLens.over { case (a, i) => g(f(a, i), i) }(s)
  def composeSourceLens(s: S): IsEq[S] = setWhatYouGet _ compose setWhatYouGet (s) <-> s
  def composeFocusLens(s: S, a: A): IsEq[A] = getWhatYouSet(s) _ compose getWhatYouSet(s) (a) <-> a
}

object AnIndexedLensLaws {
  def apply[I, S, A](_anIndexedLens: AnIndexedLens[I, S, A]): AnIndexedLensLaws[I, S, A] =
    new AnIndexedLensLaws[I, S, A] { def anIndexedLens: AnIndexedLens[I, S, A] = _anIndexedLens }
}
