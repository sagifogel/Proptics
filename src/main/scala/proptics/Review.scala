package proptics

import proptics.internal.Tagged

/** A [[Review]] is an [[Optic]] with fixed type [[Tagged]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[Review]]
 * @tparam T the modified source of an [[Review]]
 * @tparam A the target of an [[Review]]
 * @tparam B the modified target of an [[Review]]
 */
abstract class Review[S, T, A, B] {
  def apply(tagged: Tagged[A, B]): Tagged[S, T]
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
