package proptics.syntax

import proptics.{APrism, Prism}

object AsPrismSyntax {
  implicit class AsPrismOps[I, S, A](val prism: APrism[S, A]) extends AnyVal {
    def asPrism: Prism[S, A] = prism.asPrism_
  }
}
