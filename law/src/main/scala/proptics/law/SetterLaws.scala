package proptics.law

import cats.kernel.laws._
import proptics.Setter

trait SetterLaws[S, A] {
  def setter: Setter[S, A]

  def setSet(s: S, a: A): IsEq[S] = setter.set(a)(setter.set(a)(s)) <-> setter.set(a)(s)
  def setASetB(s: S, a: A, b: A): IsEq[S] = setter.set(b)(setter.set(a)(s)) <-> setter.set(b)(s)
  def overIdentity(s: S): IsEq[S] = setter.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = setter.over(g)(setter.over(f)(s)) <-> setter.over(g compose f)(s)
}

object SetterLaws {
  def apply[S, A](_setter: Setter[S, A]): SetterLaws[S, A] =
    new SetterLaws[S, A] { def setter: Setter[S, A] = _setter }
}
