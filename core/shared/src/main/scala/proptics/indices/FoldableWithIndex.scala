package proptics.indices

import scala.annotation.implicitNotFound

import cats.{Eval, Foldable, Monoid}

@implicitNotFound("Could not find an instance of FoldableWithIndex[${F}, ${I}]")
trait FoldableWithIndex[F[_], I] extends Foldable[F] {
  final def foldMapWithIndex[A, B](f: (A, I) => B)(fa: F[A])(implicit ev: Monoid[B]): B =
    foldLeftWithIndex[A, B] { case (b, (a, i)) => ev.combine(b, f(a, i)) }(fa, ev.empty)

  def foldLeftWithIndex[A, B](f: (B, (A, I)) => B)(fa: F[A], b: B): B

  def foldRightWithIndex[A, B](f: ((A, I), Eval[B]) => Eval[B])(fa: F[A], lb: Eval[B]): Eval[B]

  final override def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldMapWithIndex[A, B]((a, _) => f(a))(fa)

  final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    foldLeftWithIndex[A, B] { case (b, (a, _)) => f(b, a) }(fa, b)

  final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    foldRightWithIndex[A, B] { case ((a, _), evalB) => f(a, evalB) }(fa, lb)
}

object FoldableWithIndex {
  /** summon an instance of [[FoldableWithIndex]] for `F` */
  @inline def apply[F[_], I](implicit instance: FoldableWithIndex[F, I]): FoldableWithIndex[F, I] = instance
}
