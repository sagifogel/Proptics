package proptics.std

import scala.Function.const

import cats.syntax.either._
import cats.syntax.option._

import proptics.{Prism, Prism_}

trait OptionOptics {
  final def noneP[A, B]: Prism_[Option[A], Option[B], Unit, Unit] =
    Prism_((option: Option[A]) => option.fold(().asRight[Option[B]])(const(Option.empty[B].asLeft))) {
      const(Option.empty[B])
    }

  final def someP[A, B]: Prism_[Option[A], Option[B], A, B] =
    Prism_((option: Option[A]) => option.fold(Option.empty[B].asLeft[A])(_.asRight[Option[B]]))(_.some)

  final def none[A]: Prism[Option[A], Unit] = noneP[A, A]

  final def some[A]: Prism[Option[A], A] = someP[A, A]
}
