package optics

import cats.Id
import cats.syntax.option._
import optics.internal.Wander
import optics.Index._

import scala.Function.const

trait At[P[_, _], M, A, B] extends Index[P, M, A, B] {
  def at(a: A): Lens_[P, M, Option[B]]
}

abstract class AtInstances {
  implicit final def atIdentity[P[_, _] : Wander, A]: At[P, Id[A], Unit, A] = new At[P, Id[A], Unit, A] {
    override def at(a: Unit): Lens_[P, Id[A], Option[A]] =
      Lens_[P, Id[A], Option[A]](_.some)(a => _.getOrElse(a))

    override def ix(a: Unit): optics.Traversal_[P, Id[A], A] = indexIdentity[P, A].ix(a)
  }

  implicit final def atOption[P[_, _] : Wander, A]: At[P, Option[A], Unit, A] = new At[P, Option[A], Unit, A] {
    override def at(a: Unit): Lens_[P, Option[A], Option[A]] =
      Lens_[P, Option[A], Option[A]](identity)(const(identity))

    override def ix(a: Unit): Traversal_[P, Option[A], A] = indexOption[P, A].ix(a)
  }
}

object At extends AtInstances


