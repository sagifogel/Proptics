package proptics

import cats.syntax.eq._
import cats.syntax.option._
import cats.{Eq, Monoid}

import proptics.internal.{Forget, Indexed}
import proptics.syntax.tuple._

/** An [[IndexedGetter_]] is an [[IndexedFold_]] without a Monoid
  * <p>
  *  [[IndexedGetter_]] is just any get function (S -> (I, A))
  *  </p>
  * @tparam I the index of an [[IndexedGetter_]]
  * @tparam S the source of an [[IndexedGetter_]]
  * @tparam T the modified source of an [[IndexedGetter_]]
  * @tparam A the focus of an [[IndexedGetter_]]
  * @tparam B the modified focus of an [[IndexedGetter_]]
  */
abstract class IndexedGetter_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(indexed: Indexed[Forget[(I, A), *, *], I, A, B]): Forget[(I, A), S, T]

  /** view the focus and the index of an [[IndexedGetter_]] */
  def view(s: S): (I, A) = toForget.runForget(s)

  /** test whether a predicate holds for the focus of an [[IndexedGetter_]] */
  def exists(f: ((I, A)) => Boolean): S => Boolean = f compose view

  /** test whether a predicate does not hold for the focus of an [[IndexedGetter_]] */
  def notExists(f: ((I, A)) => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether a focus at specific index of an [[IndexedGetter_]] contains a given value */
  def contains(a: (I, A))(s: S)(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedGetter_]] does not contain a given value */
  def notContains(a: (I, A))(s: S)(implicit ev: Eq[(I, A)]): Boolean = !contains(a)(s)

  /** find if a focus of an [[IndexedGetter_]] that satisfies a predicate. */
  def find(f: ((I, A)) => Boolean): S => Option[A] = s => view(s).some.find(f).map(_._2)

  /** synonym to [[asGetter]] */
  def unIndex: Getter_[S, T, A, B] = asGetter

  /** remap the index, resulting in a change of type to the full structure */
  def reindex[J](f: ((I, A)) => (J, A)): IndexedGetter_[J, S, T, A, B] = new IndexedGetter_[J, S, T, A, B] {
    override private[proptics] def apply(indexed: Indexed[Forget[(J, A), *, *], J, A, B]): Forget[(J, A), S, T] = {
      val forget: Forget[(J, A), (I, A), B] = indexed.reindex[I](f).runIndex

      Forget(forget.runForget compose self.toForget.runForget)
    }
  }

  /** transform an [[IndexedGetter_]] to a [[Getter_]] */
  def asGetter: Getter_[S, T, A, B] = new Getter_[S, T, A, B] {
    override private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T] =
      Forget(Tuple2._2[I, A] _ compose self.view)
  }

  /** transform an [[IndexedGetter_]] to an [[IndexedFold_]] */
  def asIndexedFold: IndexedFold_[I, S, T, A, B] = new IndexedFold_[I, S, T, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
      Forget(indexed.runIndex.runForget compose self.view)
  }

  /** compose an [[IndexedGetter_]] with an [[IndexedLens_]] */
  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(I, C), *, *], I, C, D]): Forget[(I, C), S, T] =
      Forget(other.view _ compose Tuple2._2[I, A] compose self.view)
  }

  /** compose an [[IndexedGetter_]] with an [[AnIndexedLens_]] */
  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(I, C), *, *], I, C, D]): Forget[(I, C), S, T] =
      Forget(other.view _ compose Tuple2._2[I, A] compose self.view)
  }

  /** compose an [[IndexedGetter_]] with an [[IndexedTraversal_]] */
  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._2)(indexed.runIndex.runForget))
  }

  /** compose [[IndexedGetter_]] with an [[IndexedGetter_]] */
  def compose[C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedGetter_[I, S, T, C, D] = new IndexedGetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[Forget[(I, C), *, *], I, C, D]): Forget[(I, C), S, T] =
      Forget(other.view _ compose Tuple2._2[I, A] compose self.view)
  }

  /** compose [[IndexedGetter_]] with an [[IndexedFold_]] */
  def compose[C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(s => other.foldMap(self.view(s)._2)(indexed.runIndex.runForget))
  }

  private def toForget: Forget[(I, A), S, T] = self(Indexed(Forget(identity)))
}

object IndexedGetter_ {
  /** create a polymorphic [[IndexedGetter_]] from a indexed [[Forget]] function */
  private[proptics] def apply[I, S, T, A, B](f: Indexed[Forget[(I, A), *, *], I, A, B] => Forget[(I, A), S, T])(implicit ev: DummyImplicit): IndexedGetter_[I, S, T, A, B] =
    new IndexedGetter_[I, S, T, A, B] {
      override def apply(indexed: Indexed[Forget[(I, A), *, *], I, A, B]): Forget[(I, A), S, T] = f(indexed)
    }

  /** create a polymorphic [[IndexedGetter_]] from a getter function */
  def apply[I, S, T, A, B](get: S => (I, A)): IndexedGetter_[I, S, T, A, B] =
    IndexedGetter_ { indexed: Indexed[Forget[(I, A), *, *], I, A, B] => Forget[(I, A), S, T](indexed.runIndex.runForget compose get) }
}

object IndexedGetter {
  /** create a monomorphic [[IndexedGetter]] from a getter function */
  def apply[I, S, A](get: S => (I, A)): IndexedGetter[I, S, A] = IndexedGetter_(get)
}
