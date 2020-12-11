package proptics.std

import cats.syntax.either._

import proptics.{Iso, Prism, Prism_}

trait EitherOptics {
  /** extract the left element of an either using polymorphic [[Prism_]] */
  final def leftP[A, B, C]: Prism_[Either[A, C], Either[B, C], A, B] =
    Prism_ { either: Either[A, C] => either.fold(_.asRight[Either[B, C]], _.asRight[B].asLeft[A]) }(_.asLeft[C])

  /** extract the left element of an either using monomorphic [[Prism]] */
  final def left[A, B]: Prism[Either[A, B], A] = leftP[A, A, B]

  /** extract the right element of an either using polymorphic [[Prism_]] */
  final def rightP[A, B, C]: Prism_[Either[C, A], Either[C, B], A, B] =
    Prism_ { either: Either[C, A] => either.fold(_.asLeft[B].asLeft[A], _.asRight[Either[C, B]]) }(_.asRight[C])

  /** extract the right element of an either using monomorphic [[Prism]] */
  final def right[A, B]: Prism[Either[A, B], B] = rightP[B, B, A]

  /** swap the elements of an Either */
  final def swapEither[A, B]: Iso[Either[A, B], Either[B, A]] = Iso.iso[Either[A, B], Either[B, A]](_.swap)(_.swap)
}
