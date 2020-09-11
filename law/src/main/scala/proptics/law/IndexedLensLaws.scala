package proptics.law

import cats.laws._
import proptics.IndexedLens

trait IndexedLensLaws[I, S, A] {
  def indexedLens: IndexedLens[I, S, A]

  private def setWhatYouGet(s: S): S = indexedLens.set(indexedLens.view(s)._2)(s)
  private def getWhatYouSet(s: S)(a: A): A = indexedLens.view(indexedLens.set(a)(s))._2

  def setGet(s: S): IsEq[S] = setWhatYouGet(s) <-> s
  def getSet(s: S, a: A): IsEq[A] = getWhatYouSet(s)(a) <-> a
  def setSet(s: S, a: A): IsEq[S] = indexedLens.set(a)(indexedLens.set(a)(s)) <-> indexedLens.set(a)(s)
  def overIdentity(s: S): IsEq[S] = indexedLens.over(_._2)(s) <-> s
  def composeOver(s: S)(f: (I, A) => A)(g: (I, A) => A): IsEq[S] =
    indexedLens.over(g.tupled)(indexedLens.over(f.tupled)(s)) <->
      indexedLens.over { case (i, a) => g(i, f(i, a)) }(s)

  def composeSourceLens(s: S): IsEq[S] = (setWhatYouGet _ compose setWhatYouGet)(s) <-> s
  def composeFocusLens(s: S, a: A): IsEq[A] = (getWhatYouSet(s) _ compose getWhatYouSet(s))(a) <-> a
}

object IndexedLensLaws {
  def apply[I, S, A](_indexedLensLaws: IndexedLens[I, S, A]): IndexedLensLaws[I, S, A] =
    new IndexedLensLaws[I, S, A] { def indexedLens: IndexedLens[I, S, A] = _indexedLensLaws }
}
