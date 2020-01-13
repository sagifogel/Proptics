package optics.newtype

final case class Dual[A](runDual: A) extends AnyVal