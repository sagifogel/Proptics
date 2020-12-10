package proptics.std

import cats.arrow.Strong

import proptics._
import proptics.rank2types.Rank2TypeLensLike

trait TuplesOptics extends LensTupleOptics with ALensTupleOptics

private[std] trait LensTupleOptics {
  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def _1P[A, B, C]: Lens_[(A, C), (B, C), A, B] =
    Lens_(new Rank2TypeLensLike[(A, C), (B, C), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(A, C), (B, C)] = ev.first(pab)
    })

  /** synonym for [[_1P]] */
  final def fstP[A, B, C]: Lens_[(A, C), (B, C), A, B] = _1P

  /** extract the first element of a tuple using monomorphic [[Lens]] */
  final def _1[A, B]: Lens[(A, B), A] = _1P[A, A, B]

  /** synonym for [[_1]] */
  final def fst[A, B]: Lens[(A, B), A] = _1

  /** extract the second element of a tuple using polymorphic [[Lens_]] */
  final def _2P[A, B, C]: Lens_[(C, A), (C, B), A, B] =
    Lens_(new Rank2TypeLensLike[(C, A), (C, B), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(C, A), (C, B)] = ev.second(pab)
    })

  /** synonym for [[_2P]] */
  final def sndP[A, B, C]: Lens_[(C, A), (C, B), A, B] = _2P

  /** extract the first element of a tuple using monomorphic [[Lens_]] */
  final def _2[A, B]: Lens[(A, B), B] = _2P[B, B, A]

  /** synonym for [[_2]] */
  final def snd[A, B]: Lens[(A, B), B] = _2

  /** swap the elements of a Tuple */
  final def swapTuple[A, B]: Iso[(A, B), (B, A)] = Iso.iso[(A, B), (B, A)](_.swap)(_.swap)
}

private[std] trait ALensTupleOptics {
  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def _1PA[A, B, C]: ALens_[(A, C), (B, C), A, B] =
    ALens_.lens[(A, C), (B, C), A, B](_._1) { case (_, c) => b => (b, c) }

  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def fstPA[A, B, C]: ALens_[(A, C), (B, C), A, B] = _1PA

  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def _1A[A, B]: ALens[(A, B), A] = _1PA[A, A, B]

  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def fstA[A, B]: ALens[(A, B), A] = _1A

  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def _2PA[A, B, C]: ALens_[(C, A), (C, B), A, B] =
    ALens_.lens[(C, A), (C, B), A, B](_._2) { case (c, _) => b => (c, b) }

  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def sndPA[A, B, C]: ALens_[(C, A), (C, B), A, B] = _2PA

  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def _2A[A, B]: ALens[(A, B), B] = _2PA[B, B, A]

  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def sndA[A, B]: ALens[(B, A), A] = _2A
}
