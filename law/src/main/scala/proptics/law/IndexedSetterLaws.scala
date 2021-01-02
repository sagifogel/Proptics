package proptics.law

import cats.kernel.laws._

import proptics.IndexedSetter

trait IndexedSetterLaws[I, S, A] {
  def indexedSetter: IndexedSetter[I, S, A]

  def setSet(s: S, a: A): IsEq[S] = indexedSetter.set(a)(indexedSetter.set(a)(s)) <-> indexedSetter.set(a)(s)
  def setASetB(s: S, a: A, b: A): IsEq[S] = indexedSetter.set(b)(indexedSetter.set(a)(s)) <-> indexedSetter.set(b)(s)
  def overIdentity(s: S): IsEq[S] = indexedSetter.over(_._1)(s) <-> s
  def composeOver(s: S)(f: (A, I) => A)(g: (A, I) => A): IsEq[S] =
    indexedSetter.over(g.tupled)(indexedSetter.over(f.tupled)(s)) <-> indexedSetter.over({ case (a, i) =>
      g(f(a, i), i)
    })(s)
}

object IndexedSetterLaws {
  def apply[I, S, A](_indexedSetter: IndexedSetter[I, S, A]): IndexedSetterLaws[I, S, A] =
    new IndexedSetterLaws[I, S, A] { def indexedSetter: IndexedSetter[I, S, A] = _indexedSetter }
}
