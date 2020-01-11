package optics

import cats.{Id, Order}
import cats.syntax.option._
import optics.internal.Wander
import optics.Index._

import scala.Function.const

trait At[P[_, _], M, A, B] extends Index[P, M, A, B] {
  def at(a: A): Lens_[P, M, Option[B]]
}

abstract class AtInstances {
  implicit final def atIdentity[P[_, _] : Wander, A]: At[P, Id[A], Unit, A] = new At[P, Id[A], Unit, A] {
    override def at(a: Unit): Lens_[P, Id[A], Option[A]] = Lens_[P, Id[A], Option[A]](_.some)(a => _.getOrElse(a))

    override def ix(a: Unit): optics.Traversal_[P, Id[A], A] = indexIdentity[P, A].ix(a)
  }

  implicit final def atOption[P[_, _] : Wander, A]: At[P, Option[A], Unit, A] = new At[P, Option[A], Unit, A] {
    override def at(a: Unit): Lens_[P, Option[A], Option[A]] = Lens_[P, Option[A], Option[A]](identity)(const(identity))

    override def ix(a: Unit): Traversal_[P, Option[A], A] = indexOption[P, A].ix(a)
  }

  implicit final def atSet[P[_, _], A](implicit ev: Wander[P], ev2: Ordered[A]): At[P, Set[A], A, Unit] = new At[P, Set[A], A, Unit] {
    private def get(a: A)(set: Set[A]): Option[Unit] = if (set.contains(a)) ().some else None

    private def update(a: A): Set[A] => Option[Unit] => Set[A] = set => {
      case Some(_) => set - a
      case None => set + a
    }

    override def at(a: A): Lens_[P, Set[A], Option[Unit]] = Lens_[P, Set[A], Option[Unit]](get(a))(update(a))

    override def ix(a: A): Traversal_[P, Set[A], Unit] = indexSet[P, A].ix(a)
  }

  implicit final def atMap[P[_, _], K, V](implicit ev: Wander[P], ev2: Order[K]): At[P, Map[K, V], K, V] = new At[P, Map[K, V], K, V] {
    override def at(k: K): Lens_[P, Map[K, V], Option[V]] =
      Lens_[P, Map[K, V], Option[V]](_.get(k))(map => _.fold(map.removed(k))(map.updated(k, _)))

    override def ix(a: K): Traversal_[P, Map[K, V], V] = indexMap[P, K, V].ix(a)
  }
}

object At extends AtInstances


