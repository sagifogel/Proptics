package proptics

import proptics.internal.Bazaar

/**
 * A [[Traversal]] with fixed type [[Bazaar]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[ATraversal]]
 * @tparam T the modified source of a [[ATraversal]]
 * @tparam A the target of a [[ATraversal]]
 * @tparam B the modified target of a [[ATraversal]]
 */
abstract class ATraversal[S, T, A, B] {
  def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T]
}

object ATraversal {
  private[proptics] def apply[S, T, A, B](f: Bazaar[* => *, A, B, A, B] => Bazaar[* => *, A, B, S, T]): ATraversal[S, T, A, B] = new ATraversal[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = f(bazaar)
  }
}
