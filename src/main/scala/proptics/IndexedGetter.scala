package proptics

import cats.{Foldable, Monoid}
import proptics.IndexedFold_.{liftForget, replicateRank2TypeIndexedFoldLike, unfoldRank2TypeIndexedFoldLike}
import proptics.internal.{Forget, Indexed}
import proptics.rank2types.Rank2TypeIndexedFoldLike
/**
 * An [[IndexedGetter_]] is an [[IndexedFold_]]
 *
 * @tparam I the index of an [[IndexedGetter_]]
 * @tparam S the source of an [[IndexedGetter_]]
 * @tparam T the modified source of an [[IndexedGetter_]]
 * @tparam A the target of an [[IndexedGetter_]]
 * @tparam B the modified target of an [[IndexedGetter_]]
 */
abstract class IndexedGetter_[I, S, T, A, B] extends IndexedFold_[I, S, T, A, B]

object IndexedGetter_ {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedFoldLike[I, S, T, A, B]): IndexedGetter_[I, S, T, A, B] = new IndexedGetter_[I, S, T, A, B] {
    override def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] = f(indexed)
  }

  def apply[I, S, T, A, B](f: S => (I, A))(implicit ev: DummyImplicit): IndexedGetter_[I, S, T, A, B] =
    IndexedGetter_(new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      override def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
        Forget(liftForget[R, I, S, T, A, B](f)(indexed).runForget)
    })

  def replicate[A, B, T](i: Int): IndexedGetter_[Int, A, B, A, T] = IndexedGetter_(replicateRank2TypeIndexedFoldLike[A, B, T](i))

  def fromFoldable[I, F[_], A, B, T](implicit ev0: Foldable[F]): IndexedGetter_[I, F[(I, A)], B, A, T] = new IndexedGetter_[I, F[(I, A)], B, A, T] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, T]): Forget[R, F[(I, A)], B] =
      Forget[R, F[(I, A)], B](ev0.foldMap(_)(indexed.runIndex.runForget))
  }
  def unfold[I, S, T, A, B](f: S => Option[((I, A), S)]): IndexedGetter_[I, S, T, A, B] =
    IndexedGetter_(unfoldRank2TypeIndexedFoldLike[I, S, T, A, B](f))
}

object IndexedGetter {
  def apply[I, S, A](f: S => (I, A)): IndexedGetter[I, S, A] = IndexedGetter_(f)

  def fromFoldable[F[_], I, A, T](implicit ev0: Foldable[F]): IndexedGetter_[I, F[(I, A)], A, A, T] =
    IndexedGetter_.fromFoldable

  def replicate[A, T](i: Int): IndexedGetter_[Int, A, A, A, T] = IndexedGetter_.replicate(i)

  def unfold[I, S, A](f: S => Option[((I, A), S)]): IndexedGetter[I, S, A] = IndexedGetter_.unfold(f)
}
