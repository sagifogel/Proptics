package proptics.prism

import cats.syntax.option._
import cats.syntax.either._
import proptics.Prism_
import scala.{Option => Maybe}
import scala.Function.const

object Option {
  def none[A, B]: Prism_[Option[A], Option[B], Unit, Unit] =
    Prism_[Option[A], Option[B], Unit, Unit](const(Maybe.empty[B]))(const(().asRight[Option[B]]))

  def some[A, B]: Prism_[Option[A], Option[B], A, B] =
    Prism_[Option[A], Option[B], A, B](_.some)(_.fold(Maybe.empty[B].asLeft[A])(_.asRight[Option[B]]))
}
