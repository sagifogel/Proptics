package proptics.syntax

import proptics.{AnIso, Iso}

object AsIsoSyntax {
  implicit class AsIsoOps[S, A](val anIso: AnIso[S, A]) extends AnyVal {
    def asIso: Iso[S, A] = anIso.asIso_
  }
}
