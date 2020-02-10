package proptics

import cats.syntax.option._
import cats.{Id, Order}
import proptics.Index._

import scala.Function.const

trait At[M, A, B] extends Index[M, A, B] {
  def at(a: A): Lens_[M, Option[B]]
}

abstract class AtInstances {
  implicit final def atIdentity[A]: At[Id[A], Unit, A] = new At[Id[A], Unit, A] {
    override def at(a: Unit): Lens_[Id[A], Option[A]] = Lens_[Id[A], Option[A]](_.some)(a => _.getOrElse(a))

    override def ix(a: Unit): proptics.Traversal_[Id[A], A] = indexIdentity[A].ix(a)
  }

  implicit final def atOption[A]: At[Option[A], Unit, A] = new At[Option[A], Unit, A] {
    override def at(a: Unit): Lens_[Option[A], Option[A]] = Lens_[Option[A], Option[A]](identity)(const(identity))

    override def ix(a: Unit): Traversal_[Option[A], A] = indexOption[A].ix(a)
  }

  implicit final def atSet[A](implicit ev: Ordered[A]): At[Set[A], A, Unit] = new At[Set[A], A, Unit] {
    private def get(a: A)(set: Set[A]): Option[Unit] = if (set.contains(a)) ().some else None

    private def update(a: A): Set[A] => Option[Unit] => Set[A] = set => {
      case Some(_) => set - a
      case None => set + a
    }

    override def at(a: A): Lens_[Set[A], Option[Unit]] = Lens_[Set[A], Option[Unit]](get(a))(update(a))

    override def ix(a: A): Traversal_[Set[A], Unit] = indexSet[A].ix(a)
  }

  implicit final def atMap[K, V](implicit ev: Order[K]): At[Map[K, V], K, V] = new At[Map[K, V], K, V] {
    override def at(k: K): Lens_[Map[K, V], Option[V]] =
      Lens_[Map[K, V], Option[V]](_.get(k))(map => _.fold(map.removed(k))(map.updated(k, _)))

    override def ix(a: K): Traversal_[Map[K, V], V] = indexMap[ K, V].ix(a)
  }
}

object At extends AtInstances


