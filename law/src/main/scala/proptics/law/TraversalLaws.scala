package proptics.law

import cats.kernel.laws._
import cats.{Applicative, Id}

import proptics.Traversal

trait TraversalLaws[S, A] {
  def traversal: Traversal[S, A]
  def respectPurity[F[_]: Applicative](s: S): IsEq[F[S]] = traversal.traverse[F](s)(Applicative[F].pure _) <-> Applicative[F].pure(s)
  def consistentFoci(s: S, f: A => A, g: A => A): IsEq[S] =
    traversal.overF[Id](f) _ compose traversal.overF[Id](g) (s) <-> traversal.overF[Id](f compose g)(s)

  def preview(s: S): IsEq[Option[A]] = traversal.preview(s) <-> traversal.viewAll(s).headOption
  def getSet(s: S, f: A => A): IsEq[List[A]] = traversal.viewAll(traversal.over(f)(s)) <-> traversal.viewAll(s).map(f)
  def setSet(s: S, a: A): IsEq[S] = traversal.set(a)(traversal.set(a)(s)) <-> traversal.set(a)(s)
  def overIdentity(s: S): IsEq[S] = traversal.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = traversal.over(g)(traversal.over(f)(s)) <-> traversal.over(g compose f)(s)
}

object TraversalLaws {
  def apply[S, A](_traversal: Traversal[S, A]): TraversalLaws[S, A] =
    new TraversalLaws[S, A] { def traversal: Traversal[S, A] = _traversal }
}
