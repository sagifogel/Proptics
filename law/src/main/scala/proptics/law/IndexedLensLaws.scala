package proptics.law

import cats.laws._

import proptics.IndexedLens

trait IndexedLensLaws[I, S, A] {
  def indexedLens: IndexedLens[I, S, A]

  private def setWhatYouGet(s: S): S = indexedLens.set(indexedLens.view(s)._1)(s)
  private def getWhatYouSet(s: S)(a: A): A = indexedLens.view(indexedLens.set(a)(s))._1

  def setGet(s: S): IsEq[S] = setWhatYouGet(s) <-> s
  def getSet(s: S, a: A): IsEq[A] = getWhatYouSet(s)(a) <-> a
  def setSet(s: S, a: A): IsEq[S] = indexedLens.set(a)(indexedLens.set(a)(s)) <-> indexedLens.set(a)(s)
  def overIdentity(s: S): IsEq[S] = indexedLens.over(_._1)(s) <-> s
  def composeOver(s: S)(f: (A, I) => A)(g: (A, I) => A): IsEq[S] =
    indexedLens.over(g.tupled)(indexedLens.over(f.tupled)(s)) <->
      indexedLens.over { case (a, i) => g(f(a, i), i) }(s)

  def composeSourceLens(s: S): IsEq[S] = setWhatYouGet _ compose setWhatYouGet (s) <-> s
  def composeFocusLens(s: S, a: A): IsEq[A] = getWhatYouSet(s) _ compose getWhatYouSet(s) (a) <-> a
}

object IndexedLensLaws {
  def apply[I, S, A](_indexedLensLaws: IndexedLens[I, S, A]): IndexedLensLaws[I, S, A] =
    new IndexedLensLaws[I, S, A] { def indexedLens: IndexedLens[I, S, A] = _indexedLensLaws }
}
