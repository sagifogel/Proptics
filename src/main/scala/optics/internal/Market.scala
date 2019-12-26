package optics.internal

/** The [[Market]] profunctor characterizes an [[optics.Prism]] */
final case class Market[A, B, S, T](to: B => T, from: S => Either[T, A])