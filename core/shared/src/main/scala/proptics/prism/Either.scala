package proptics.prism

import cats.syntax.either._
import proptics.Prism_

object Either {
  def left[A, B, C]: Prism_[Either[A, C], Either[B, C], A, B] =
    Prism_((b: B) => b.asLeft[C])(_.fold(_.asRight[Either[B, C]], _.asRight[B].asLeft[A]))

  def right[A, B, C]: Prism_[Either[C, A], Either[C, B], A, B] =
    Prism_((b: B) => b.asRight[C])(_.fold(_.asLeft[B].asLeft[A], _.asRight[Either[C, B]]))
}
