package proptics.syntax

import proptics.{AnIso_, Iso}

object AsIsoSyntax {
  implicit class AsIsoOps[S, A](val anIso: AnIso_[S, A]) extends AnyVal {
    def asIso: Iso[S, A] = anIso.asIso_
  }
}
