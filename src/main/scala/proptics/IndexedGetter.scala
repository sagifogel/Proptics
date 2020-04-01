package proptics

import cats.{Foldable, Monoid}
import proptics.IndexedFold_.{liftForget, replicateRank2TypeIndexedFoldLike, unfoldRank2TypeIndexedFoldLike}
import proptics.internal.{Forget, Indexed}
import proptics.rank2types.Rank2TypeIndexedFoldLike
/**
 * An [[IndexedGetter]] is an [[IndexedFold_]]
 *
 * @tparam I the index of an [[IndexedGetter]]
 * @tparam S the source of an [[IndexedGetter]]
 * @tparam T the modified source of an [[IndexedGetter]]
 * @tparam A the target of an [[IndexedGetter]]
 * @tparam B the modified target of an [[IndexedGetter]]
 */
abstract class IndexedGetter[I, S, T, A, B] extends IndexedFold_[I, S, T, A, B]

object IndexedGetter {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedFoldLike[I, S, T, A, B]): IndexedGetter[I, S, T, A, B] = new IndexedGetter[I, S, T, A, B] {
    override def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] = f(indexed)
  }

  def apply[I, S, T, A, B](f: S => (I, A))(implicit ev: DummyImplicit): IndexedGetter[I, S, T, A, B] =
    IndexedGetter(new Rank2TypeIndexedFoldLike[I, S, T, A, B] {
      override def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T] =
        Forget(liftForget[R, I, S, T, A, B](f)(indexed).runForget)
    })

  def replicate[A, B, T](i: Int): IndexedGetter[Int, A, B, A, T] = IndexedGetter(replicateRank2TypeIndexedFoldLike[A, B, T](i))

  def fromFoldable[I, F[_], A, B, T](implicit ev0: Foldable[F]): IndexedGetter[I, F[(I, A)], B, A, T] = new IndexedGetter[I, F[(I, A)], B, A, T] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, T]): Forget[R, F[(I, A)], B] =
      Forget[R, F[(I, A)], B](ev0.foldMap(_)(indexed.runIndex.runForget))
  }
  def unfold[I, S, T, A, B](f: S => Option[((I, A), S)]): IndexedGetter[I, S, T, A, B] =
    IndexedGetter(unfoldRank2TypeIndexedFoldLike[I, S, T, A, B](f))
}

object IndexedGetter_ {
  def apply[I, S, A](f: S => (I, A)): IndexedGetter_[I, S, A] = IndexedGetter(f)

  def fromFoldable[F[_], I, A, T](implicit ev0: Foldable[F]): IndexedGetter[I, F[(I, A)], A, A, T] =
    IndexedGetter.fromFoldable

  def replicate[A, T](i: Int): IndexedGetter[Int, A, A, A, T] = IndexedGetter.replicate(i)

  def unfold[I, S, A](f: S => Option[((I, A), S)]): IndexedGetter_[I, S, A] = IndexedGetter.unfold(f)
}
