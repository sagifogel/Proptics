package proptics.prism

import cats.syntax.option._
import cats.syntax.either._
import proptics.{Prism, Prism_}

import scala.{Option => Maybe}
import scala.Function.const

object Option {
  def none_[A, B]: Prism_[Option[A], Option[B], Unit, Unit] =
    Prism_ { _: Option[A] => ().asRight[Option[B]] }(const(Maybe.empty[B]))

  def some_[A, B]: Prism_[Option[A], Option[B], A, B] =
    Prism_ { option: Option[A] => option.fold(Maybe.empty[B].asLeft[A])(_.asRight[Option[B]]) }(_.some)

  def none[A, B]: Prism[Option[A], Unit] =
    Prism[Option[A], Unit](_.asLeft[Unit])(const(Maybe.empty[A]))

  def some[A, B]: Prism[Option[A], A] =
    Prism[Option[A], A](_.fold(Maybe.empty[A].asLeft[A])(_.asRight[Option[A]]))(_.some)
}
