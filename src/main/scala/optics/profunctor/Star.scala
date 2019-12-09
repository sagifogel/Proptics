package optics.profunctor

final case class Star[F[_], A, B](runStar: A => F[B])

abstract class StarInstances {
}

object Star extends StarInstances