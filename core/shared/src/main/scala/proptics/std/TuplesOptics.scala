package proptics.std

import cats.arrow.Strong

import proptics.rank2types.Rank2TypeLensLike
import proptics.{ALens, ALens_, Lens, Lens_}

trait TuplesOptics extends LensTupleOptics with ALensTupleOptics

private[std] trait LensTupleOptics {
  final def _1P[A, B, C]: Lens_[(A, C), (B, C), A, B] =
    Lens_(new Rank2TypeLensLike[(A, C), (B, C), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(A, C), (B, C)] = ev.first(pab)
    })

  final def fstP[A, B, C]: Lens_[(A, C), (B, C), A, B] = _1P

  final def _1[A, B]: Lens[(A, B), A] = _1P[A, A, B]

  final def fst[A, B]: Lens[(A, B), A] = _1

  final def _2P[A, B, C]: Lens_[(C, A), (C, B), A, B] =
    Lens_(new Rank2TypeLensLike[(C, A), (C, B), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(C, A), (C, B)] = ev.second(pab)
    })

  final def sndP[A, B, C]: Lens_[(C, A), (C, B), A, B] = _2P

  final def _2[A, B]: Lens[(A, B), B] = _2P[B, B, A]

  final def snd[A, B]: Lens[(A, B), B] = _2
}

private[std] trait ALensTupleOptics {
  final def _1PA[A, B, C]: ALens_[(A, C), (B, C), A, B] =
    ALens_.lens[(A, C), (B, C), A, B](_._1) { case (_, c) => b => (b, c) }

  final def fstPA[A, B, C]: ALens_[(A, C), (B, C), A, B] = _1PA

  final def _1A[A, B]: ALens[(A, B), A] = _1PA[A, A, B]

  final def fstA[A, B]: ALens[(A, B), A] = _1A

  final def _2PA[A, B, C]: ALens_[(C, A), (C, B), A, B] =
    ALens_.lens[(C, A), (C, B), A, B](_._2) { case (c, _) => b => (c, b) }

  final def sndPA[A, B, C]: ALens_[(C, A), (C, B), A, B] = _2PA

  final def _2A[A, B]: ALens[(A, B), B] = _2PA[B, B, A]

  final def sndA[A, B]: ALens[(B, A), A] = _2A
}
