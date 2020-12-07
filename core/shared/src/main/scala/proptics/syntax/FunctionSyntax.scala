package proptics.syntax

trait FunctionSyntax {
  implicit final def function2FlipOps[A, B, C](f: A => B => C): Function2FlipOps[A, B, C] = Function2FlipOps(f)

  implicit final def functionFlippedApply[A](a: A): FunctionFlippedApply[A] = FunctionFlippedApply(a)
}

final case class Function2FlipOps[A, B, C](private val f: A => B => C) extends AnyVal {
  /** flip the order of the arguments to a function of two arguments */
  def flip: B => A => C = b => a => f(a)(b)

  /** transforms a unary function returning another unary function into a function of arity 2 */
  def uncurried: (A, B) => C = Function.uncurried(f)
}

final case class FunctionFlippedApply[A](private val a: A) extends AnyVal {
  /** applies an argument to a function */
  def applyFlipped[B](f: A => B): B = f(a)

  /** synonym to [[applyFlipped]] */
  def &[B](f: A => B): B = applyFlipped(f)
}
