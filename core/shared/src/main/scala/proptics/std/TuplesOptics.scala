package proptics.std

import cats.arrow.Strong

import proptics._
import proptics.rank2types.Rank2TypeLensLike
import proptics.typeclass.{Field1, Field2}

trait TuplesOptics extends LensTupleOptics with ALensTupleOptics

private[std] trait LensTupleOptics {
  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def _1P[A, B, C]: Lens_[(A, C), (B, C), A, B] =
    Lens_(new Rank2TypeLensLike[(A, C), (B, C), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(A, C), (B, C)] = ev.first(pab)
    })

  /** synonym for [[_1P]] */
  final def firstP[A, B, C]: Lens_[(A, C), (B, C), A, B] = _1P

  /** extract the first element of a tuple using monomorphic [[Lens]] */
  final def _1[A, B](implicit ev: Field1[(A, B), A]): Lens[(A, B), A] = ev.first

  /** extract the second element of a tuple using polymorphic [[Lens_]] */
  final def _2P[A, B, C]: Lens_[(C, A), (C, B), A, B] =
    Lens_(new Rank2TypeLensLike[(C, A), (C, B), A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[(C, A), (C, B)] = ev.second(pab)
    })

  /** synonym for [[_2P]] */
  final def secondP[A, B, C]: Lens_[(C, A), (C, B), A, B] = _2P

  /** extract the second element of a tuple using monomorphic [[Lens_]] */
  final def _2[A, B](implicit ev: Field2[(A, B), B]): Lens[(A, B), B] = ev.second

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
  final def _1A[A, B](implicit ev: Field1[(A, B), A]): ALens[(A, B), A] = ev.first.asALens

  /** extract the second element of a tuple using polymorphic [[Lens_]] */
  final def _2PA[A, B, C]: ALens_[(C, A), (C, B), A, B] =
    ALens_.lens[(C, A), (C, B), A, B](_._2) { case (c, _) => b => (c, b) }

  /** extract the second element of a tuple using polymorphic [[Lens_]] */
  final def secondPA[A, B, C]: ALens_[(C, A), (C, B), A, B] = _2PA

  /** extract the second element of a tuple using polymorphic [[Lens_]] */
  final def _2A[A, B](implicit ev: Field2[(A, B), B]): ALens[(A, B), B] = ev.second.asALens

  /** extract the first element of a tuple using polymorphic [[Lens_]] */
  final def secondA[A, B](implicit ev: Field2[(A, B), B]): ALens[(A, B), B] = _2A
}
