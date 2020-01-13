package proptics.newtype

final case class Disj[A](runDisj: A) extends AnyVal