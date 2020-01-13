package optics.syntax

object FunctionSyntax {
  implicit class Function2Ops[A, B, C](val f: A => B => C) extends AnyVal {
    def flip: B => A => C = b => a => f(a)(b)
  }

  implicit class Function2Domain[A, B, C](val f: B => B => C) extends AnyVal {
    def on(g: A => B): A => A => C = x => y => f(g(x))(g(y))
  }
}