package optics.syntax

import optics.Optic

import scala.Function.const

object OpticSyntax {
  implicit class OpticOps[S, T, A, B](val optic: Optic[* => *, S, T, A, B]) extends AnyVal {
    def over(f: A => B): S => T = optic(f)

    def set(b: B): S => T = over(const(b))
  }
}
