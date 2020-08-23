package proptics.law

import cats.kernel.laws._
import proptics.IndexedSetter

final case class IndexedSetterLaws[I, S, A](indexedSetter: IndexedSetter[I, S, A]) extends AnyVal {
  def setSet(s: S, a: A): IsEq[S] = indexedSetter.set(a)(indexedSetter.set(a)(s)) <-> indexedSetter.set(a)(s)
  def setASetB(s: S, a: A, b: A): IsEq[S] = indexedSetter.set(b)(indexedSetter.set(a)(s)) <-> indexedSetter.set(b)(s)
  def overIdentity(s: S): IsEq[S] = indexedSetter.over(_._2)(s) <-> s
  def composeOver(s: S)(f: (I, A) => A)(g: (I, A) => A): IsEq[S] =
    indexedSetter.over(g.tupled)(indexedSetter.over { case (i, a) => f(i, a) }(s)) <-> indexedSetter.over({
      case (i, a) => g(i, f(i, a))
    })(s)
}
