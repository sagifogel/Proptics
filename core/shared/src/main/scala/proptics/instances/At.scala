package proptics.instances

import cats.syntax.option._
import cats.{Id, Order}
import proptics.{AffineTraversal, Lens}
import proptics.instances.index._
import scala.Function.const

/**
  * [[At]] provides a [[Lens]] that can be used to read, write or delete the value associated with a key in a Map-like container
  */
trait At[M, A, B] extends Index[M, A, B] {
  def at(a: A): Lens[M, Option[B]]
}

trait AtInstances {
  implicit final def atIdentity[A]: At[Id[A], Unit, A] = new At[Id[A], Unit, A] {
    override def at(a: Unit): Lens[Id[A], Option[A]] = Lens { id: Id[A] => id.some }(a => _.getOrElse(a))

    override def ix(a: Unit): AffineTraversal[Id[A], A] = indexIdentity[A].ix(a)
  }

  implicit final def atOption[A]: At[Option[A], Unit, A] = new At[Option[A], Unit, A] {
    override def at(a: Unit): Lens[Option[A], Option[A]] = Lens { op: Option[A] => op }(const(identity))

    override def ix(a: Unit): AffineTraversal[Option[A], A] = indexOption[A].ix(a)
  }

  implicit final def atSet[A](implicit ev: Ordered[A]): At[Set[A], A, Unit] = new At[Set[A], A, Unit] {
    private def get(a: A)(set: Set[A]): Option[Unit] = if (set.contains(a)) ().some else None

    private def update(a: A): Set[A] => Option[Unit] => Set[A] = set => {
      case Some(_) => set - a
      case None    => set + a
    }

    override def at(a: A): Lens[Set[A], Option[Unit]] = Lens[Set[A], Option[Unit]](get(a) _)(update(a))

    override def ix(a: A): AffineTraversal[Set[A], Unit] = indexSet[A].ix(a)
  }

  implicit final def atMap[K, V](implicit ev: Order[K]): At[Map[K, V], K, V] = new At[Map[K, V], K, V] {
    override def at(k: K): Lens[Map[K, V], Option[V]] =
      Lens { map: Map[K, V] => map.get(k) }(map => _.fold(map - k)(map.updated(k, _)))

    override def ix(a: K): AffineTraversal[Map[K, V], V] = indexMap[K, V].ix(a)
  }
}