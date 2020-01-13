package optics.newtype

final case class Conj[A](runConj: A) extends AnyVal
