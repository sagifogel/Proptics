package proptics.instances

import cats.arrow.Strong
import proptics.Lens_
import proptics.rank2types.Rank2TypeLensLike

trait TuplesInstances {
  final def _1[A, B, C]: Lens_[(A, C), (B, C), A, B] =
    Lens_(new Rank2TypeLensLike[(A, C), (B, C), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(A, C), (B, C)] = ev.first(pab)
    })

  final def fst[A, B, C]: Lens_[(A, C), (B, C), A, B] = _1

  final def _2[A, B, C]: Lens_[(C, A), (C, B), A, B] =
    Lens_(new Rank2TypeLensLike[(C, A), (C, B), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(C, A), (C, B)] = ev.second(pab)
    })

  final def snd[A, B, C]: Lens_[(C, A), (C, B), A, B] = _2
}
