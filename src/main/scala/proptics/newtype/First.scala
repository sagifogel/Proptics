package proptics.newtype

final case class First[A](runFirst: Option[A]) extends AnyVal
