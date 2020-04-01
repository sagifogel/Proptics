package proptics.syntax

import proptics.{AGrate, Grate}

object AsGrateSyntax {
  implicit class AsPrismOps[S, A](val grate: AGrate[S, A]) extends AnyVal {
    def asGrate: Grate[S, A] = grate.asGrate_
  }
}
