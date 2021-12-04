package proptics.macros

import proptics.AppliedLens

trait AppliedLensSyntaxCompat {
  implicit final def appliedLensOps[S](s: S): AppliedLensOps[S] = new AppliedLensOps(s)
}

class AppliedLensOps[S](private val s: S) extends AnyVal {
  def lens[A](field: S => A): AppliedLens[S, A] = macro AppliedLensMacro.genAppliedLens_impl[S, A]
}
