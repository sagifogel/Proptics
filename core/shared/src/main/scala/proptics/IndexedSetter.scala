package proptics

import cats.instances.function._
import proptics.internal.Indexed

import scala.Function.const

/**
  * An [[IndexedSetter_]] is an indexed optic. A generalization of indexed fmap.
  *
  * @tparam S the source of an [[IndexedSetter_]]
  * @tparam I the index of an [[IndexedSetter_]]
  * @tparam T the modified source of an [[IndexedSetter_]]
  * @tparam A the target of an [[IndexedSetter_]]
  * @tparam B the modified target of an [[IndexedSetter_]]
  */
abstract class IndexedSetter_[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): S => T

  def set(b: B): S => T = over(const(b))

  def over(f: ((I, A)) => B): S => T = self(Indexed(f))

  def compose[C, D](other: IndexedLens_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T = {
      val a2b = other(indexed)
      self(Indexed { case (_, a) => a2b(a) })
    }
  }

  def compose[C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = self compose other.asIndexedLens_

  def compose[C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T = {
      val a2b = other(indexed)
      self(Indexed { case (_, a) => a2b(a) })
    }
  }
}

object IndexedSetter_ {
  private[proptics] def apply[I, S, T, A, B](f: Indexed[* => *, I, A, B] => S => T): IndexedSetter_[I, S, T, A, B] = new IndexedSetter_[I, S, T, A, B] {
    override def apply(indexed: Indexed[* => *, I, A, B]): S => T = f(indexed)
  }

  def apply[I, S, T, A, B](get: ((I, A) => B) => S => T)(implicit ev: DummyImplicit): IndexedSetter_[I, S, T, A, B] =
    IndexedSetter_ { indexed: Indexed[* => *, I, A, B] =>
      get { case (i, a) => indexed.runIndex(i, a) }
    }
}

object IndexedSetter {
  def apply[I, S, A](get: ((I, A) => A) => S => S): IndexedSetter[I, S, A] =
    IndexedSetter_ { indexed: Indexed[* => *, I, A, A] => get { case (i, a) => indexed.runIndex(i, a) } }
}
