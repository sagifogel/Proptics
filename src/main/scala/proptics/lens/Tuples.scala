package proptics.lens

import cats.arrow.Strong
import proptics.Lens
import proptics.rank2types.Rank2TypeLensLike

object Tuples {
  def _1[A, B, C]: Lens[(A, C), (B, C), A, B] = Lens(new Rank2TypeLensLike[(A, C), (B, C), A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(A, C), (B, C)] = ev.first(pab)
  })

  def fst[A, B, C]: Lens[(A, C), (B, C), A, B] = _1

  def _2[A, B, C]: Lens[(C, A), (C, B), A, B] = Lens(new Rank2TypeLensLike[(C, A), (C, B), A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(C, A), (C, B)] = ev.second(pab)
  })

  def snd[A, B, C]: Lens[(C, A), (C, B), A, B] = _2
}