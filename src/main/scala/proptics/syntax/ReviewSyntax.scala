package proptics.syntax

import proptics.Optic
import proptics.internal.Tagged

object ReviewSyntax {
  implicit class ReviewOps[S, T, A, B](val _review: Optic[Tagged, S, T, A, B]) extends AnyVal {
    def review(b: B): T = _review(Tagged[A, B](b)).runTag
  }
}
