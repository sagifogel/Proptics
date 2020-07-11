package proptics.law

import cats.syntax.option._
import cats.instances.option._
import cats.kernel.laws._
import cats.Id
import proptics.Traversal

final case class TraversalLaws[S, A](traversal: Traversal[S, A]) extends AnyVal {
  def respectPurity(s: S): IsEq[Option[S]] = traversal.traverse(s)(_.some) <-> s.some

  def consistentFoci(s: S, f: A => A, g: A => A): IsEq[S] =
    (traversal.overF[Id](f) _ compose traversal.overF[Id](g))(s) <-> traversal.overF[Id](f compose g)(s)

  def preview(s: S): IsEq[Option[A]] = traversal.preview(s) <-> traversal.viewAll(s).headOption
  def setGet(s: S, f: A => A): IsEq[List[A]] = traversal.viewAll(traversal.over(f)(s)) <-> traversal.viewAll(s).map(f)
  def setSet(s: S, a: A): IsEq[S] = traversal.set(a)(traversal.set(a)(s)) <-> traversal.set(a)(s)
  def overIdentity(s: S): IsEq[S] = traversal.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = traversal.over(g)(traversal.over(f)(s)) <-> traversal.over(g compose f)(s)
}
