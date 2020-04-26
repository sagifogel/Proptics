package proptics

import proptics.internal.Tagged

/** A [[Review_]] is an Optic with fixed type [[Tagged]] [[cats.arrow.Profunctor]]
  *  A [[Review_]] describes how to construct a single value.
  *
  * @tparam S the source of a [[Review_]]
  * @tparam T the modified source of a [[Review_]]
  * @tparam A the target of a [[Review_]]
  * @tparam B the modified target of a [[Review_]]
  */
abstract class Review_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(tagged: Tagged[A, B]): Tagged[S, T]

  def review(b: B): T = self(Tagged[A, B](b)).runTag

  def compose[C, D](other: Iso_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]) = self(other(tagged)(Tagged.profunctorTagged))
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): Review_[S, T, C, D] = self compose other.asIso

  def compose[C, D](other: Prism_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]) = self(other(tagged))
  }

  def compose[C, D](other: APrism_[A, B, C, D]): Review_[S, T, C, D] = self compose other.asPrism

  def compose[C, D](other: Grate_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]) = self(other(tagged))
  }

  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]) = self(other(tagged))
  }
}

object Review_ {
  private[proptics] def apply[S, T, A, B](f: Tagged[A, B] => Tagged[S, T]): Review_[S, T, A, B] = new Review_[S, T, A, B] {
    override def apply(pab: Tagged[A, B]): Tagged[S, T] = f(pab)
  }

  def apply[S, T, A, B](f: B => T)(implicit ev: DummyImplicit): Review_[S, T, A, B] =
    Review_((tag: Tagged[A, B]) => Tagged[S, T](f(tag.runTag)))
}

object Review {
  def apply[S, A](f: A => S): Review[S, A] = Review_(f)
}
