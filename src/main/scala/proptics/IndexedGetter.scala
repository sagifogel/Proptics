package proptics

import proptics.IndexedFold.liftForget
import proptics.internal.{Forget, Indexed}

import scala.Function.const

/**
 * An [[IndexedGetter]] is an [[IndexedFold]]
 *
 * @tparam I the index of an [[IndexedGetter]]
 * @tparam S the source of an [[IndexedGetter]]
 * @tparam T the modified source of an [[IndexedGetter]]
 * @tparam A the target of an [[IndexedGetter]]
 * @tparam B the modified target of an [[IndexedGetter]]
 */
abstract class IndexedGetter[I, S, T, A, B] extends IndexedFold[A, I, S, T, A, B] { self =>
  def asGetter: Getter[S, T, A, B] = new Getter[S, T, A, B] {
    override private[proptics] def apply[R](forget: Forget[R, A, B]): Forget[R, S, T] = {
      ???
//      Forget[R, S, T](s => {
//        self(Indexed(Forget { case (_, a: A) => forget.runForget(a) } )).runForget(s)
//      })
    }
  }
}

object IndexedGetter {
  def apply[I, S, T, A, B](f: Indexed[Forget[A, *, *], I, A, B] => Forget[A, S, T]): IndexedGetter[I, S, T, A, B] = new IndexedGetter[I, S, T, A, B] {
    override def apply(indexed: Indexed[Forget[A, *, *], I, A, B]): Forget[A, S, T] = f(indexed)
  }

  def apply[I, S, T, A, B](f: S => (I, A))(implicit ev: DummyImplicit): IndexedGetter[I, S, T, A, B] =
    IndexedGetter(liftForget[A, I, S, T, A, B](f))

  def to[I, S, T, A, B](f: S => (I, A)): IndexedGetter[I, S, T, A, B] = IndexedGetter(f)
}

object IndexedGetter_ {
  def apply[I, S, A](f: S => (I, A)): IndexedGetter_[I, S, A] = to(f)

  def to[I, S, T, A, B](f: S => (I, A)): IndexedGetter_[I, S, A] = IndexedGetter(f)
}
