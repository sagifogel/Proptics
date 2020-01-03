package optics.newtype

final case class Last[A](runLast: Option[A]) extends AnyVal