package proptics.prism

import cats.syntax.either._
import proptics.Prism_

object Either {
  def left[A, B, C]: Prism_[Either[A, C], Either[B, C], A, B] =
    Prism_[Either[C, A], Either[B, C], A, B](_.asLeft[C])(_.bimap[Either[C, B], A](_.asLeft[B], identity))

  def right[A, B, C]: Prism_[Either[C, A], Either[C, B], A, B] =
    Prism_[Either[C, A], Either[C, B], A, B](_.asRight[C])(_.bimap[Either[C, B], A](_.asLeft[B], identity))
}
