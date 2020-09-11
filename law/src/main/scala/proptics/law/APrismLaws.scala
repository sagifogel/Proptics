package proptics.law

import cats.kernel.laws._
import cats.syntax.option._
import proptics.APrism

trait APrismLaws[S, A] {
  def aPrism: APrism[S, A]

  def previewReview(a: A): IsEq[Option[A]] = aPrism.preview(aPrism.review(a)) <-> a.some
  def viewOrModifyReview(s: S): IsEq[S] = aPrism.viewOrModify(s).fold(identity, aPrism.review) <-> s
  def setSet(s: S, a: A): IsEq[S] = aPrism.set(a)(aPrism.set(a)(s)) <-> aPrism.set(a)(s)
  def overIdentity(s: S): IsEq[S] = aPrism.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = aPrism.over(g)(aPrism.over(f)(s)) <-> aPrism.over(g compose f)(s)
}

object APrismLaws {
  def apply[S, A](_aPrism: APrism[S, A]): APrismLaws[S, A] =
    new APrismLaws[S, A] { def aPrism: APrism[S, A] = _aPrism }
}
