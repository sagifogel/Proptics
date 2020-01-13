package optics.newtype

final case class Endo[C[_, _], A](runEndo: C[A, A]) extends AnyVal
