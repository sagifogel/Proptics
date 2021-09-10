package proptics.law

import scala.Function.const

import cats.kernel.laws._
import cats.{Applicative, Id, catsInstancesForId}

import proptics.AnAffineTraversal

trait AnAffineTraversalLaws[S, A] {
  def anAffineTraversal: AnAffineTraversal[S, A]
  def respectPurity[F[_]: Applicative](s: S): IsEq[F[S]] =
    anAffineTraversal.traverse(s)(Applicative[F].pure _) <-> Applicative[F].pure(s)

  def consistentFoci(s: S, f: A => A, g: A => A): IsEq[S] =
    (anAffineTraversal.overF[Id](f) _ compose anAffineTraversal.overF[Id](g))(s) <-> anAffineTraversal.overF[Id](f compose g)(s)

  def getSet(s: S): IsEq[S] = anAffineTraversal.viewOrModify(s).fold(identity, anAffineTraversal.set(_)(s)) <-> s
  def previewSet(s: S, a: A): IsEq[Option[A]] =
    anAffineTraversal.preview(anAffineTraversal.set(a)(s)) <-> anAffineTraversal.preview(s).map(const(a))
  def setSet(s: S, a: A): IsEq[S] = anAffineTraversal.set(a)(anAffineTraversal.set(a)(s)) <-> anAffineTraversal.set(a)(s)
  def overIdentity(s: S): IsEq[S] = anAffineTraversal.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = anAffineTraversal.over(g)(anAffineTraversal.over(f)(s)) <-> anAffineTraversal.over(g compose f)(s)
}

object AnAffineTraversalLaws {
  def apply[S, A](_anAffineTraversal: AnAffineTraversal[S, A]): AnAffineTraversalLaws[S, A] =
    new AnAffineTraversalLaws[S, A] { def anAffineTraversal: AnAffineTraversal[S, A] = _anAffineTraversal }
}
