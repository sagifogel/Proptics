package proptics

import proptics.internal.Tagged

/** A [[Review]] is an Optic with fixed type [[Tagged]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[Review]]
 * @tparam T the modified source of a [[Review]]
 * @tparam A the target of a [[Review]]
 * @tparam B the modified target of a [[Review]]
 */
abstract class Review[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(tagged: Tagged[A, B]): Tagged[S, T]

  def review(b: B): T = self(Tagged[A, B](b)).runTag
}

object Review {
  private[proptics] def apply[S, T, A, B](f: Tagged[A, B] => Tagged[S, T]): Review[S, T, A, B] = new Review[S, T, A, B] {
    override def apply(pab: Tagged[A, B]): Tagged[S, T] = f(pab)
  }

  def apply[S, T, A, B](f: B => T)(implicit ev: DummyImplicit): Review[S, T, A, B] =
    Review((tag: Tagged[A, B]) => Tagged[S, T](f(tag.runTag)))
}

object Review_ {
  def apply[S, A](f: A => S): Review_[S, A] = Review(f)
}
