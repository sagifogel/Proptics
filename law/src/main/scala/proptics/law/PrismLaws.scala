package proptics.law

import cats.syntax.option._
import cats.kernel.laws._
import proptics.Prism

trait PrismLaws[S, A] {
  def prism: Prism[S, A]

  def previewReview(a: A): IsEq[Option[A]] = prism.preview(prism.review(a)) <-> a.some
  def viewOrModifyReview(s: S): IsEq[S] = prism.viewOrModify(s).fold(identity, prism.review) <-> s
  def setSet(s: S, a: A): IsEq[S] = prism.set(a)(prism.set(a)(s)) <-> prism.set(a)(s)
  def overIdentity(s: S): IsEq[S] = prism.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = prism.over(g)(prism.over(f)(s)) <-> prism.over(g compose f)(s)
}

object PrismLaws {
  def apply[S, A](_prism: Prism[S, A]): PrismLaws[S, A] =
    new PrismLaws[S, A] { def prism: Prism[S, A] = _prism }
}
