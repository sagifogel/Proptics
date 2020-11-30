package proptics.std

import cats.arrow.Strong

import proptics.rank2types.Rank2TypeLensLike
import proptics.{ALens, ALens_, Lens, Lens_}

trait TuplesOptics {
  final def _p1[A, B, C]: Lens_[(A, C), (B, C), A, B] =
    Lens_(new Rank2TypeLensLike[(A, C), (B, C), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(A, C), (B, C)] = ev.first(pab)
    })

  final def pfst[A, B, C]: Lens_[(A, C), (B, C), A, B] = _p1

  final def _1[A, B]: Lens[(A, B), A] = _p1[A, A, B]

  final def fst[A, B]: Lens[(A, B), A] = _1

  final def _p1A[A, B, C]: ALens_[(A, C), (B, C), A, B] =
    ALens_.lens[(A, C), (B, C), A, B](_._1) { case (_, c) => b => (b, c) }

  final def pfsta[A, B, C]: ALens_[(A, C), (B, C), A, B] = _p1A

  final def _1A[A, B]: ALens[(A, B), A] = _p1A[A, A, B]

  final def fstA[A, B]: ALens[(A, B), A] = _1A

  final def _p2[A, B, C]: Lens_[(C, A), (C, B), A, B] =
    Lens_(new Rank2TypeLensLike[(C, A), (C, B), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(C, A), (C, B)] = ev.second(pab)
    })

  final def psnd[A, B, C]: Lens_[(C, A), (C, B), A, B] = _p2

  final def _2[A, B]: Lens[(B, A), A] = _p2[A, A, B]

  final def snd[A, B]: Lens[(B, A), A] = _2

  final def _p2A[A, B, C]: ALens_[(C, A), (C, B), A, B] =
    ALens_.lens[(C, A), (C, B), A, B](_._2) { case (c, _) => b => (c, b) }

  final def psndA[A, B, C]: ALens_[(C, A), (C, B), A, B] = _p2A

  final def _2A[A, B]: ALens[(B, A), A] = _p2A[A, A, B]

  final def sndA[A, B]: ALens[(B, A), A] = _2A
}
