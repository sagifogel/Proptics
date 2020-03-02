package proptics

import proptics.internal.{Forget, Indexed}

import scala.Function.uncurried

/**
 * A [[IndexedFold]] is an [[IndexedOptic]] with fixed type [[Forget]] [[cats.arrow.Profunctor]]
 *
 * @tparam R the return type of an [[IndexedFold]]
 * @tparam I the index of an [[IndexedFold]]
 * @tparam S the source of an [[IndexedFold]]
 * @tparam T the modified source of an [[IndexedFold]]
 * @tparam A the target of an [[IndexedFold]]
 * @tparam B the modified target of an [[IndexedFold]]
 */
abstract class IndexedFold[R, I, S, T, A, B] { self =>
  private[proptics] def apply(indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]

  def foldMapOf(f: I => A => R)(s: S): R = self(Indexed(Forget(uncurried(f).tupled))).runForget(s)
}

object IndexedFold {
  private[proptics] def apply[R, I, S, T, A, B](f: Indexed[Forget[R, *, *], I, A, B] => Forget[R, S, T]): IndexedFold[R, I, S, T, A, B] = new IndexedFold[R, I, S, T, A, B] {
    override def apply(indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] = f(indexed)
  }

  def apply[R, I, S, T, A, B](f: S => (I, A))(implicit ev: DummyImplicit): IndexedFold[R, I, S, T, A, B] =
    IndexedFold(liftForget[R, I, S, T, A, B](f))

  private[proptics] def liftForget[R, I, S, T, A, B](f: S => (I, A)): Indexed[Forget[R, *, *], I, A, B] => Forget[R, S, T] =
    indexed => Forget[R, S, T](indexed.runIndex.runForget compose f)
}

object IndexedFold_ {
  def apply[R, I, S, A](f: S => (I, A)): IndexedFold_[R, I, S, A] = IndexedFold(f)
}
