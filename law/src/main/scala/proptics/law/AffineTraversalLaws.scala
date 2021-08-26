package proptics.law

import scala.Function.const

import cats.kernel.laws._
import cats.catsInstancesForId
import cats.{Applicative, Id}

import proptics.AffineTraversal

trait AffineTraversalLaws[S, A] {
  def affineTraversal: AffineTraversal[S, A]
  def respectPurity[F[_]: Applicative](s: S): IsEq[F[S]] =
    affineTraversal.traverse(s)(Applicative[F].pure _) <-> Applicative[F].pure(s)

  def consistentFoci(s: S, f: A => A, g: A => A): IsEq[S] =
    (affineTraversal.overF[Id](f) _ compose affineTraversal.overF[Id](g))(s) <-> affineTraversal.overF[Id](f compose g)(s)

  def getSet(s: S): IsEq[S] = affineTraversal.viewOrModify(s).fold(identity, affineTraversal.set(_)(s)) <-> s
  def previewSet(s: S, a: A): IsEq[Option[A]] =
    affineTraversal.preview(affineTraversal.set(a)(s)) <-> affineTraversal.preview(s).map(const(a))
  def setSet(s: S, a: A): IsEq[S] = affineTraversal.set(a)(affineTraversal.set(a)(s)) <-> affineTraversal.set(a)(s)
  def overIdentity(s: S): IsEq[S] = affineTraversal.over(identity)(s) <-> s
  def composeOver(s: S)(f: A => A)(g: A => A): IsEq[S] = affineTraversal.over(g)(affineTraversal.over(f)(s)) <-> affineTraversal.over(g compose f)(s)
}

object AffineTraversalLaws {
  def apply[S, A](_affineTraversal: AffineTraversal[S, A]): AffineTraversalLaws[S, A] =
    new AffineTraversalLaws[S, A] { def affineTraversal: AffineTraversal[S, A] = _affineTraversal }
}
