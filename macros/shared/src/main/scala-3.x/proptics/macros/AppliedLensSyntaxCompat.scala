package proptics.macros

import proptics.AppliedLens

trait AppliedLensSyntaxCompat {
  extension [S](inline s: S) {
    inline def lens[A](inline field: S => A): AppliedLens[S, A] = ${ AppliedLensImpl[S, A]('s, 'field) }
  }
}
