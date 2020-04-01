package proptics.syntax

import proptics.{AGrate_, Grate}

object AsGrateSyntax {
  implicit class AsPrismOps[S, A](val grate: AGrate_[S, A]) extends AnyVal {
    def asGrate: Grate[S, A] = grate.asGrate_
  }
}
