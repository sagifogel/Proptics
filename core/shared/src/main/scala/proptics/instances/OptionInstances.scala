package proptics.instances

import cats.syntax.either._
import cats.syntax.option._
import proptics.{Prism, Prism_}

import scala.Function.const

trait OptionInstances {
  final def none_[A, B]: Prism_[Option[A], Option[B], Unit, Unit] =
    Prism_ { _: Option[A] => ().asRight[Option[B]] }(const(Option.empty[B]))

  final def some_[A, B]: Prism_[Option[A], Option[B], A, B] =
    Prism_ { option: Option[A] => option.fold(Option.empty[B].asLeft[A])(_.asRight[Option[B]]) }(_.some)

  final def none[A]: Prism[Option[A], Unit] =
    Prism[Option[A], Unit](_.asLeft[Unit])(const(Option.empty[A]))

  final def some[A]: Prism[Option[A], A] =
    Prism[Option[A], A](_.fold(Option.empty[A].asLeft[A])(_.asRight[Option[A]]))(_.some)
}
