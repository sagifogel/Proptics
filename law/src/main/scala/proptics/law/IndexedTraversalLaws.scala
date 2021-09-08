package proptics.law

import cats.kernel.laws._
import cats.{Applicative, Id, catsInstancesForId}

import proptics.{IndexedTraversal, IndexedTraversal_}

trait IndexedTraversalLaws[I, S, A] {
  def indexedTraversal: IndexedTraversal_[I, S, S, A, A]

  def respectPurity[F[_]: Applicative](s: S): IsEq[F[S]] =
    indexedTraversal.traverse[F](s) { case (a, _) => Applicative[F].pure(a) } <-> Applicative[F].pure(s)

  def consistentFoci(s: S, f: (A, I) => A, g: (A, I) => A): IsEq[S] =
    (indexedTraversal.overF[Id](f.tupled) _ compose indexedTraversal.overF[Id](g.tupled))(s) <->
      indexedTraversal.overF[Id]({ case (a, i) => f(g(a, i), i) })(s)

  def preview(s: S): IsEq[Option[(A, I)]] = indexedTraversal.preview(s) <-> indexedTraversal.viewAll(s).headOption

  def getSet(s: S, f: (A, I) => A): IsEq[List[(A, I)]] =
    indexedTraversal.viewAll(indexedTraversal.over(f.tupled)(s)) <->
      indexedTraversal.viewAll(s).map { case (a, i) => (f(a, i), i) }

  def setSet(s: S, a: A): IsEq[S] = indexedTraversal.set(a)(indexedTraversal.set(a)(s)) <-> indexedTraversal.set(a)(s)
  def overIdentity(s: S): IsEq[S] = indexedTraversal.over(_._1)(s) <-> s

  def composeOver(s: S)(f: (A, I) => A)(g: (A, I) => A): IsEq[S] =
    indexedTraversal.over(g.tupled)(indexedTraversal.over(f.tupled)(s)) <-> indexedTraversal.over({ case (a, i) =>
      g(f(a, i), i)
    })(s)
}

object IndexedTraversalLaws {
  def apply[I, S, A](_indexedTraversal: IndexedTraversal[I, S, A]): IndexedTraversalLaws[I, S, A] =
    new IndexedTraversalLaws[I, S, A] { def indexedTraversal: IndexedTraversal[I, S, A] = _indexedTraversal }
}
