package optics

import optics.internal.Tagged

/** A [[Review]] is an [[Optic]] with fixed type [[Tagged]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[Review]]
 * @tparam T the modified source of an [[Review]]
 * @tparam A the target of an [[Review]]
 * @tparam B the modified target of an [[Review]]
 */
abstract class Review[S, T, A, B] extends Optic[Tagged, S, T, A, B] {
}

object Review {
  private[optics] def apply[S, T, A, B](f: Tagged[A, B] => Tagged[S, T]): Review[S, T, A, B] = new Review[S, T, A, B] {
    override def apply(pab: Tagged[A, B]): Tagged[S, T] = f(pab)
  }

  def apply[S, T, A, B](f: B => T)(implicit ev: DummyImplicit): Review[S, T, A, B] =
    Review((tag: Tagged[A, B]) => Tagged[S, T](f(tag.runTag)))
}
