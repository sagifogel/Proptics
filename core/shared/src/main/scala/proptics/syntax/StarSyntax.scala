package proptics.syntax

import proptics.profunctor.Star

trait StarSyntax {
  implicit final def starOps[F[_], A, B](star: Star[F, A, B]): StarOps[F, A, B] = StarOps(star)
}

final case class StarOps[F[_], -A, B](private val star: Star[F, A, B]) extends AnyVal {
  def runStar(a: A): F[B] = star.run(a)
}
