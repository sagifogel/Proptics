package proptics.syntax

import proptics.Optic
import proptics.internal.Zipping

object ZippingSyntax {
  implicit class ZippingOps[P[_, _], S, T, A, B](val zipping: Optic[Zipping, S, T, A, B]) extends AnyVal {
    def zipWithOf(f: A => A => B) : S => S => T = zipping(Zipping(f)).runZipping
  }
}
