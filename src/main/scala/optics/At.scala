package optics

import cats.Id
import cats.syntax.option._
import optics.internal.Wander
import optics.Index._

trait At[P[_, _], A, B, C] extends Index[P, A, B, C] {
  def at(a: A): Lens_[P, A, Option[C]]
}

abstract class AtInstances {
  implicit final def atIdentity[P[_, _] : Wander, A]: At[P, Id[A], Unit, A] = new At[P, Id[A], Unit, A] {
    override def at(a: Id[A]): Lens_[P, Id[A], Option[A]] =
      Lens_[P, Id[A], Option[A]](_.some)(a => _.getOrElse(a))

    override def ix(f: Id[A]): Traversal_[P, Id[A], A] = indexIdentity[P, A].ix(f)
  }
}

object At extends AtInstances


