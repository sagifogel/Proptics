package optics.internal

/** Profunctor used for `[[IndexedOptic]]`s. */
final class Indexed[P[_, _], I, S, T](runIndex: P[(I, S), T])

abstract class IndexedInstances {

}

object Indexed extends IndexedInstances