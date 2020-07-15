package proptics.instances

import cats.arrow.Strong
import proptics.rank2types.Rank2TypeLensLike
import proptics.{ALens_, Lens_}

trait TuplesInstances {
  final def _1[A, B, C]: Lens_[(A, C), (B, C), A, B] =
    Lens_(new Rank2TypeLensLike[(A, C), (B, C), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(A, C), (B, C)] = ev.first(pab)
    })

  final def fst[A, B, C]: Lens_[(A, C), (B, C), A, B] = _1

  final def fstA[A, B, C]: ALens_[(A, C), (B, C), A, B] = _1A

  final def _1A[A, B, C]: ALens_[(A, C), (B, C), A, B] =
    ALens_.lens[(A, C), (B, C), A, B](_._1) { case (_, c) => b => (b, c) }

  final def _2[A, B, C]: Lens_[(C, A), (C, B), A, B] =
    Lens_(new Rank2TypeLensLike[(C, A), (C, B), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(C, A), (C, B)] = ev.second(pab)
    })

  final def _2A[A, B, C]: ALens_[(C, A), (C, B), A, B] =
    ALens_.lens[(C, A), (C, B), A, B](_._2) { case (c, _) => b => (c, b) }

  final def snd[A, B, C]: Lens_[(C, A), (C, B), A, B] = _2

  final def sndA[A, B, C]: ALens_[(C, A), (C, B), A, B] = _2A
}
