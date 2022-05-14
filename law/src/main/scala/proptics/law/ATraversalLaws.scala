package proptics.law

import cats.Id
import cats.kernel.laws._
import cats.syntax.option._

import proptics.ATraversal

trait ATraversalLaws[S, A] {
  def aTraversal: ATraversal[S, A]
  def respectPurity(s: S): IsEq[Option[S]] = aTraversal.traverse(s)(_.some) <-> s.some

  def consistentFoci(s: S, f: A => A, g: A => A): IsEq[S] =
    aTraversal.overF[Id](f) _ compose aTraversal.overF[Id](g) (s) <-> aTraversal.overF[Id](f compose g)(s)

  def preview(s: S): IsEq[Option[A]] = aTraversal.preview(s) <-> aTraversal.viewAll(s).headOption
  def setGet(s: S, f: A => A): IsEq[List[A]] = aTraversal.viewAll(aTraversal.over(f)(s)) <-> aTraversal.viewAll(s).map(f)
  def setSet(s: S, a: A): IsEq[S] = aTraversal.set(a)(aTraversal.set(a)(s)) <-> aTraversal.set(a)(s)
  def overIdentity(s: S): IsEq[S] = aTraversal.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = aTraversal.over(g)(aTraversal.over(f)(s)) <-> aTraversal.over(g compose f)(s)
}

object ATraversalLaws {
  def apply[S, A](_aTraversal: ATraversal[S, A]): ATraversalLaws[S, A] =
    new ATraversalLaws[S, A] { def aTraversal: ATraversal[S, A] = _aTraversal }
}
