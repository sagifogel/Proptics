package proptics

import cats.syntax.eq._
import cats.syntax.option._
import cats.{Eq, Monoid}
import proptics.internal.{Forget, Indexed}
import proptics.syntax.tuple._

/**
  * An [[IndexedGetter_]] is an [[IndexedFold_]]
  *
  * @tparam I the index of an [[IndexedGetter_]]
  * @tparam S the source of an [[IndexedGetter_]]
  * @tparam T the modified source of an [[IndexedGetter_]]
  * @tparam A the target of an [[IndexedGetter_]]
  * @tparam B the modified target of an [[IndexedGetter_]]
  */
abstract class IndexedGetter_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(indexed: Indexed[Forget[(I, A), *, *], I, A, B]): Forget[(I, A), S, T]

  def view(s: S): (I, A) = self(Indexed(Forget(identity))).runForget(s)

  def exists(f: ((I, A)) => Boolean): S => Boolean = f compose view

  def contains(a: (I, A))(s: S)(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  def notContains(a: (I, A))(s: S)(implicit ev: Eq[(I, A)]): Boolean = !contains(a)(s)

  def find(f: ((I, A)) => Boolean): S => Option[(I, A)] = s => view(s).some.find(f)

  def asGetter: Getter_[S, T, A, B] = new Getter_[S, T, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T] =
      Forget(Tuple2._2[I, A] _ compose self.view)
  }

  def asIndexedFold: IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]) =
      Forget(indexed.runIndex.runForget compose self.view)
  }

  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(I, C), *, *], I, C, D]): Forget[(I, C), S, T] =
      Forget(other.view _ compose Tuple2._2[I, A] compose self.view)
  }

  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._2)(indexed.runIndex.runForget))
  }

  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(I, C), *, *], I, C, D]): Forget[(I, C), S, T] =
      Forget(other.view _ compose Tuple2._2[I, A] compose self.view)
  }

  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._2)(indexed.runIndex.runForget))
  }
}

object IndexedGetter_ {
  private[proptics] def apply[I, S, T, A, B](f: Indexed[Forget[(I, A), *, *], I, A, B] => Forget[(I, A), S, T]): IndexedGetter_[I, S, T, A, B] =
    new IndexedGetter_[I, S, T, A, B] {
      override def apply(indexed: Indexed[Forget[(I, A), *, *], I, A, B]): Forget[(I, A), S, T] = f(indexed)
    }

  def apply[I, S, T, A, B](f: S => (I, A))(implicit ev: DummyImplicit): IndexedGetter_[I, S, T, A, B] =
    IndexedGetter_ { indexed: Indexed[Forget[(I, A), *, *], I, A, B] => Forget[(I, A), S, T](indexed.runIndex.runForget compose f) }
}

object IndexedGetter {
  def apply[I, S, A](f: S => (I, A)): IndexedGetter[I, S, A] = IndexedGetter_(f)
}
