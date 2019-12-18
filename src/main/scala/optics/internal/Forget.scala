package optics.internal

/** [[cats.arrow.Profunctor]] that forgets the [[B]] value and returns (and accumulates) a value of type [[R]]. */
final case class Forget[R, A, B](runForget: A => R)