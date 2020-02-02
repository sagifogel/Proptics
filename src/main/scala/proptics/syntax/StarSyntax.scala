package proptics.syntax

import cats.Alternative
import proptics.Optic
import proptics.newtype.Disj
import proptics.profunctor.Star

object StarSyntax {
  implicit class TraverseOfStarOps[P[_, _], F[_], S, T, A, B](val traverse: Optic[Star[F, *, *], S, T, A, B]) extends AnyVal {
    def traverseOf(f: A => F[B])(s: S): F[T] = traverse(Star(f)).runStar(s)
  }

  implicit class SequenceOfstarOps[P[_, _], F[_], S, T, A, B](val traverse: Optic[Star[F, *, *], S, T, F[A], A]) extends AnyVal {
    def sequenceOf(s: S): F[T] = traverse.traverseOf(identity)(s)
  }

  implicit class StarOps[F[_], S, T, A, B](val star: Optic[Star[F, *, *], S, T, A, B]) extends AnyVal {
    def collectOf(f: A => F[B]): S => F[T] = star(Star(f)).runStar
  }

  implicit class StarDisjOps[F[_], S, T, A, B](val starOptic: Optic[Star[(Disj[Boolean], *), *, *], S, T, A, B]) extends AnyVal {
    def failover(f: A => B)(s: S)(implicit ev: Alternative[F]): F[T] = {
      val star = Star[(Disj[Boolean], *), A, B](a => (Disj(true), f(a)))
      starOptic(star).runStar(s) match {
        case (Disj(true), x) => ev.pure(x)
        case (Disj(false), _) => ev.empty
      }
    }
  }
}
