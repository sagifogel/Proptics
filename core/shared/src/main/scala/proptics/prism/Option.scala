package proptics.prism

import cats.syntax.option._
import cats.syntax.either._
import proptics.{Prism, Prism_}

import scala.{Option => Maybe}
import scala.Function.const

object Option {
  def none_[A, B]: Prism_[Option[A], Option[B], Unit, Unit] =
    Prism_((_: Unit) => Maybe.empty[B])(const(().asRight[Option[B]]))

  def some_[A, B]: Prism_[Option[A], Option[B], A, B] =
    Prism_((b: B) => b.some)(_.fold(Maybe.empty[B].asLeft[A])(_.asRight[Option[B]]))

  def none[A, B]: Prism[Option[A], Unit] =
    Prism[Option[A], Unit](const(Maybe.empty[A]))(_.asLeft[Unit])

  def some[A, B]: Prism[Option[A], A] =
    Prism[Option[A], A](_.some)(_.fold(Maybe.empty[A].asLeft[A])(_.asRight[Option[A]]))
}
