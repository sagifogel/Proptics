package proptics.syntax

trait FunctionSyntax {
  implicit final def function2FlipOps[A, B, C](f: A => B => C): Function2FlipOps[A, B, C] = Function2FlipOps(f)

  implicit final def function2Domain[A, B, C](f: B => B => C): Function2Domain[A, B, C] = Function2Domain(f)

  implicit final def functionFlippedApply[A](a: A): FunctionFlippedApply[A] = FunctionFlippedApply(a)
}

final case class Function2FlipOps[A, B, C](private val f: A => B => C) extends AnyVal {
  def flip: B => A => C = b => a => f(a)(b)

  def uncurried: (A, B) => C = Function.uncurried(f)
}

final case class Function2Domain[A, B, C](private val f: B => B => C) extends AnyVal {
  def on(g: A => B): A => A => C = x => y => f(g(x))(g(y))
}

final case class FunctionFlippedApply[A](private val a: A) extends AnyVal {
  def applyFlipped[B](f: A => B): B = f(a)

  def `#`[B](f: A => B): B = applyFlipped(f)
}
