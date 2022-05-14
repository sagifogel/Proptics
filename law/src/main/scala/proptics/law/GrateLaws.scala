package proptics.law

import cats.Id
import cats.kernel.laws._

import proptics.Grate
import proptics.profunctor.Closed._

trait GrateLaws[S, A] {
  def grate: Grate[S, A]
  private[this] def identityGrate: Grate[A, A] = Grate.id[A]

  def identityLaw(a: A): IsEq[Id[A]] =
    identityGrate(identity[A] _)(closedFunction)(a) <-> a

  def consistentFoci(s: S, f: A => A, g: A => A): IsEq[S] =
    grate(f)(closedFunction) compose grate(g) (s) <->
      grate(f compose g)(closedFunction)(s)

  def setSet(s: S, a: A): IsEq[S] = grate.set(a)(grate.set(a)(s)) <-> grate.set(a)(s)
  def overIdentity(s: S): IsEq[S] = grate.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = grate.over(g)(grate.over(f)(s)) <-> grate.over(g compose f)(s)
}

object GrateLaws {
  def apply[S, A](_grate: Grate[S, A]): GrateLaws[S, A] =
    new GrateLaws[S, A] { def grate: Grate[S, A] = _grate }
}
