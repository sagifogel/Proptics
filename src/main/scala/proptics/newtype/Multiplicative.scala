package optics.newtype

final case class Multiplicative[A](runMultiplicative: A) extends AnyVal
