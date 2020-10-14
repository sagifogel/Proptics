package proptics.law

import cats.kernel.laws._
import cats.{Applicative, Id}
import proptics.{IndexedTraversal, IndexedTraversal_}

trait IndexedTraversalLaws[I, S, A] {
  def indexedTraversal: IndexedTraversal_[I, S, S, A, A]

  def respectPurity[F[_]: Applicative](s: S): IsEq[F[S]] =
    indexedTraversal.traverse[F](s) { case (_, a) => Applicative[F].pure(a) } <-> Applicative[F].pure(s)

  def consistentFoci(s: S, f: (I, A) => A, g: (I, A) => A): IsEq[S] =
    (indexedTraversal.overF[Id](f.tupled) _ compose indexedTraversal.overF[Id](g.tupled))(s) <->
      indexedTraversal.overF[Id]({ case (i, a) => f(i, g(i, a)) })(s)

  def preview(s: S): IsEq[Option[(I, A)]] = indexedTraversal.preview(s) <-> indexedTraversal.viewAll(s).headOption

  def getSet(s: S, f: (I, A) => A): IsEq[List[(I, A)]] =
    indexedTraversal.viewAll(indexedTraversal.over { case (i, a) => f(i, a) }(s)) <->
      indexedTraversal.viewAll(s).map { case (i, a) => (i, f(i, a)) }

  def setSet(s: S, a: A): IsEq[S] = indexedTraversal.set(a)(indexedTraversal.set(a)(s)) <-> indexedTraversal.set(a)(s)
  def overIdentity(s: S): IsEq[S] = indexedTraversal.over(_._2)(s) <-> s

  def composeOver(s: S)(f: (I, A) => A)(g: (I, A) => A): IsEq[S] =
    indexedTraversal.over(g.tupled)(indexedTraversal.over { case (i, a) => f(i, a) }(s)) <-> indexedTraversal.over({ case (i, a) =>
      g(i, f(i, a))
    })(s)
}

object IndexedTraversalLaws {
  def apply[I, S, A](_indexedTraversal: IndexedTraversal[I, S, A]): IndexedTraversalLaws[I, S, A] =
    new IndexedTraversalLaws[I, S, A] { def indexedTraversal: IndexedTraversal[I, S, A] = _indexedTraversal }
}
