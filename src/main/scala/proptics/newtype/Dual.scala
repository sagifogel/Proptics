package proptics.newtype

final case class Dual[A](runDual: A) extends AnyVal