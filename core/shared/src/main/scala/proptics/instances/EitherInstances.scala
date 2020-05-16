package proptics.instances

import cats.syntax.either._
import proptics.Prism_

trait EitherInstances {
  final def left[A, B, C]: Prism_[Either[A, C], Either[B, C], A, B] =
    Prism_ { either: Either[A, C] => either.fold(_.asRight[Either[B, C]], _.asRight[B].asLeft[A]) }(_.asLeft[C])

  final def right[A, B, C]: Prism_[Either[C, A], Either[C, B], A, B] =
    Prism_ { either: Either[C, A] => either.fold(_.asLeft[B].asLeft[A], _.asRight[Either[C, B]]) }(_.asRight[C])
}
