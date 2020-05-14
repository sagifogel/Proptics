package proptics

import cats.instances.function._
import proptics.internal.Indexed
import proptics.syntax.tuple._

import scala.Function.{const, untupled}

/**
  * An [[IndexedSetter_]] is an indexed optic. A generalization of indexed fmap.
  *
  * @tparam S the source of an [[IndexedSetter_]]
  * @tparam I the index of an [[IndexedSetter_]]
  * @tparam T the modified source of an [[IndexedSetter_]]
  * @tparam A the focus an [[IndexedSetter_]]
  * @tparam B the modified focus of an [[IndexedSetter_]]
  */
abstract class IndexedSetter_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): S => T

  /** set the modified focus of an [[IndexedSetter_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the focus type of an [[IndexedSetter_]] using a function, resulting in a change of type to the full structure  */
  def over(f: ((I, A)) => B): S => T = self(Indexed(f))

  /** synonym to [[asSetter]] */
  def unIndex: Setter_[S, T, A, B] = asSetter

  /** transforms an [[IndexedSetter_]] to a [[Setter_]] */
  def asSetter: Setter_[S, T, A, B] = new Setter_[S, T, A, B] {
    override private[proptics] def apply(pab: A => B) =
      self(Indexed(pab compose Tuple2._2[I, A]))
  }

  /** compose [[IndexedSetter_]] with an [[IndexedLens_]] */
  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed(other(indexed) compose Tuple2._2[I, A]))
  }

  /** compose [[IndexedSetter_]] with an [[AnIndexedLens_]] */
  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = self compose other.asIndexedLens

  /** compose [[IndexedSetter_]] with an [[IndexedTraversal_]] */
  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(Indexed(other(indexed) compose Tuple2._2[I, A]))
  }

  /** compose [[IndexedSetter_]] with an [[IndexedSetter_]] */
  def compose[C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]) =
      self(Indexed(other(indexed) compose Tuple2._2[I, A]))
  }
}

object IndexedSetter_ {
  /** create a polymorphic [[IndexedSetter_]] from an Indexed over function */
  private[proptics] def apply[I, S, T, A, B](_over: Indexed[* => *, I, A, B] => S => T): IndexedSetter_[I, S, T, A, B] = new IndexedSetter_[I, S, T, A, B] {
    override def apply(indexed: Indexed[* => *, I, A, B]): S => T = _over(indexed)
  }

  /** create a polymorphic [[IndexedSetter_]] from an [[IndexedSetter_.over]] function */
  def apply[I, S, T, A, B](get: ((I, A) => B) => S => T)(implicit ev: DummyImplicit): IndexedSetter_[I, S, T, A, B] =
    IndexedSetter_ { indexed: Indexed[* => *, I, A, B] => get(untupled(indexed.runIndex)) }
}

object IndexedSetter {
  /** create a monomorphic [[IndexedSetter]] from an [[IndexedSetter_.over]] function */
  def apply[I, S, A](get: ((I, A) => A) => S => S): IndexedSetter[I, S, A] =
    IndexedSetter_ { indexed: Indexed[* => *, I, A, A] => get(untupled(indexed.runIndex)) }
}
