package optics.syntax

object FunctionSyntax {
  implicit class Function2Ops[A, B, C](val fn: A => B => C) extends AnyVal {
    def flip: B => A => C = b => a => fn(a)(b)
  }
}