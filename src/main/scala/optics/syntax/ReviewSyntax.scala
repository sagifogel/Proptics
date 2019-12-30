package optics.syntax

import optics.Optic
import optics.internal.Tagged

object ReviewSyntax {
  implicit class ReviewOps[S, T, A, B](val optic: Optic[Tagged, S, T, A, B]) extends AnyVal {
      def review(b: B): T = optic(Tagged[A, B](b)).runTag
  }
}
