package proptics

import proptics.internal.Tagged

/** A [[Review_]] is an Optic with fixed type [[Tagged]] [[cats.arrow.Profunctor]]
  * <p>
  * [[Review_]] describes how to construct a single value It's a dual of [[Getter]]
  * </p>
  * <p>
  * [[Review_]] is isomorphic to an arbitrary function from (B -> T)
  * </p>
  * @tparam S the source of a [[Review_]]
  * @tparam T the modified source of a [[Review_]]
  * @tparam A the focus of a [[Review_]]
  * @tparam B the modified focus of a [[Review_]]
  */
abstract class Review_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(tagged: Tagged[A, B]): Tagged[S, T]

  /** view the modified source of a [[Review_]] */
  def review(b: B): T = self(Tagged[A, B](b)).runTag

  /** compose [[Review_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged)(Tagged.profunctorTagged))
  }

  /** compose [[Review_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): Review_[S, T, C, D] = self compose other.asIso

  /** compose [[Review_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))
  }

  /** compose [[Review_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): Review_[S, T, C, D] = self compose other.asPrism

  /** compose [[Review_]] with a [[Grate_]] */
  def compose[C, D](other: Grate_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))
  }

  /** compose [[Review_]] with a [[Review_]] */
  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))
  }
}

object Review_ {

  /** create a polymorphic Review_ from a a synonym to [[Review_.apply]] */
  private[proptics] def apply[S, T, A, B](f: Tagged[A, B] => Tagged[S, T]): Review_[S, T, A, B] = new Review_[S, T, A, B] {
    override def apply(pab: Tagged[A, B]): Tagged[S, T] = f(pab)
  }

  /** create a polymorphic Review_ from a preview function */
  def apply[S, T, A, B](review: B => T)(implicit ev: DummyImplicit): Review_[S, T, A, B] =
    Review_ { tag: Tagged[A, B] => Tagged[S, T](review(tag.runTag)) }

  /** polymorphic identity of a [[Review_]] */
  def id[S, T]: Review_[S, T, S, T] = Review_[S, T, S, T](identity[T] _)
}

object Review {

  /** create a monomorphic [[Review]] from a preview function */
  def apply[S, A](f: A => S): Review[S, A] = Review_(f)

  /** monomorphic identity of a [[Review]] */
  def id[S]: Review[S, S] = Review_.id[S, S]
}
