package proptics.profunctor

final case class Costar[F[_], B, A](runCostar: F[B] => A)

abstract class CostarInstances {
}

object Costar extends CostarInstances

