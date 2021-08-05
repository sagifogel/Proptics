package proptics

import cats.data.State

import proptics.internal.Tagged

/** A [[Review_]] describes how to construct a single value. It's a dual of [[Getter_]].
  *
  * A [[Review_]] is isomorphic to an arbitrary function from (B -> T)
  *
  * @tparam S the source of a [[Review_]]
  * @tparam T the modified source of a [[Review_]]
  * @tparam A the focus of a [[Review_]]
  * @tparam B the modified focus of a [[Review_]]
  */
abstract class Review_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(tagged: Tagged[A, B]): Tagged[S, T]

  /** view the modified source of a [[Review_]] */
  final def review(b: B): T = self(Tagged[A, B](b)).runTag

  /** view the modified focus of a [[Review_]] in the state of a monad */
  final def reuse(implicit ev: State[B, T]): State[B, T] = ev.inspect(review)

  /** compose this [[Review_]] with an [[Iso_]], having this [[Review_]] applied last */
  final def compose[C, D](other: Iso_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged)(Tagged.profunctorTagged))
  }

  /** compose this [[Review_]] with an [[Iso_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: Iso_[C, D, S, T]): Review_[C, D, A, B] = new Review_[C, D, A, B] {
    override private[proptics] def apply(tagged: Tagged[A, B]): Tagged[C, D] =
      Tagged(other.review(self.review(tagged.runTag)))
  }

  /** compose this [[Review_]] with an [[AnIso_]], having this [[Review_]] applied last */
  final def compose[C, D](other: AnIso_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] =
      Tagged(self.review(other.review(tagged.runTag)))
  }

  /** compose this [[Review_]] with an [[AnIso_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: AnIso_[C, D, S, T]): Review_[C, D, A, B] = new Review_[C, D, A, B] {
    override private[proptics] def apply(tagged: Tagged[A, B]): Tagged[C, D] =
      Tagged(other.review(self.review(tagged.runTag)))
  }

  /** compose this [[Review_]] with a [[Prism_]], having this [[Review_]] applied last */
  final def compose[C, D](other: Prism_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))
  }

  /** compose this [[Review_]] with a [[Prism_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: Prism_[C, D, S, T]): Review_[C, D, A, B] = new Review_[C, D, A, B] {
    override private[proptics] def apply(tagged: Tagged[A, B]): Tagged[C, D] =
      Tagged(other.review(self.review(tagged.runTag)))
  }

  /** compose this [[Review_]] with an [[APrism_]], having this [[Review_]] applied last */
  final def compose[C, D](other: APrism_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] =
      Tagged(self.review(other.review(tagged.runTag)))
  }

  /** compose this [[Review_]] with an [[APrism_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: APrism_[C, D, S, T]): Review_[C, D, A, B] = new Review_[C, D, A, B] {
    override private[proptics] def apply(tagged: Tagged[A, B]): Tagged[C, D] =
      Tagged(other.review(self.review(tagged.runTag)))
  }

  /** compose this [[Review_]] with a [[Grate_]], having this [[Review_]] applied last */
  final def compose[C, D](other: Grate_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))
  }

  /** compose this [[Review_]] with a [[Grate_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: Grate_[C, D, S, T]): Review_[C, D, A, B] = new Review_[C, D, A, B] {
    override private[proptics] def apply(tagged: Tagged[A, B]): Tagged[C, D] =
      Tagged(other.review(self.review(tagged.runTag)))
  }

  /** compose this [[Review_]] with a [[Review_]], having this [[Review_]] applied last */
  final def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]): Tagged[S, T] = self(other(tagged))
  }

  /** compose this [[Review_]] with a [[Review_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: Review_[C, D, S, T]): Review_[C, D, A, B] = new Review_[C, D, A, B] {
    override private[proptics] def apply(tagged: Tagged[A, B]): Tagged[C, D] =
      Tagged(other.review(self.review(tagged.runTag)))
  }
}

object Review_ {
  /** create a polymorphic Review_ from a a synonym to [[Review_.apply]] */
  private[proptics] def apply[S, T, A, B](f: Tagged[A, B] => Tagged[S, T])(implicit ev: DummyImplicit): Review_[S, T, A, B] =
    new Review_[S, T, A, B] {
      override def apply(pab: Tagged[A, B]): Tagged[S, T] = f(pab)
    }

  /** create a polymorphic Review_ from a preview function */
  final def apply[S, T, A, B](review: B => T): Review_[S, T, A, B] =
    Review_ { tag: Tagged[A, B] => Tagged[S, T](review(tag.runTag)) }

  /** polymorphic identity of a [[Review_]] */
  final def id[S, T]: Review_[S, T, S, T] = Review_[S, T, S, T](identity[T] _)
}

object Review {
  /** create a monomorphic [[Review]] from a preview function */
  final def apply[S, A](f: A => S): Review[S, A] = Review_(f)

  /** monomorphic identity of a [[Review]] */
  final def id[S]: Review[S, S] = Review_.id[S, S]
}
