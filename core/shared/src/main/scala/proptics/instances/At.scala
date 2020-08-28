package proptics.instances

import cats.syntax.option._
import cats.{Id, Order}
import proptics.instances.index._
import proptics.instances.option._
import proptics.{AffineTraversal, Lens}

import scala.Function.const

/**
  * [[At]] provides a [[Lens]] that can be used to read, write or delete the value associated with a key in a Map-like container
  */
trait At[S, I, A] extends Index[S, I, A] {
  def at(i: I): Lens[S, Option[A]]
}

trait AtInstances {
  def at[S, I, A](i: I)(implicit ev: At[S, I, A]): Lens[S, Option[A]] = ev.at(i)

  def apply[S, I, A](get: I => S => Option[A])(set: I => S => Option[A] => S): At[S, I, A] = new At[S, I, A] {
    override def at(i: I): Lens[S, Option[A]] = Lens[S, Option[A]](get(i))(set(i))

    override def ix(i: I): AffineTraversal[S, A] =
      at(i) compose (some[A] compose AffineTraversal.id[A])
  }

  implicit final def atIdentity[A]: At[Id[A], Unit, A] = new At[Id[A], Unit, A] {
    override def at(i: Unit): Lens[Id[A], Option[A]] = Lens { id: Id[A] => id.some }(a => _.getOrElse(a))

    override def ix(i: Unit): AffineTraversal[Id[A], A] = indexIdentity[A].ix(i)
  }

  implicit final def atOption[A]: At[Option[A], Unit, A] = new At[Option[A], Unit, A] {
    override def at(i: Unit): Lens[Option[A], Option[A]] = Lens { op: Option[A] => op }(const(identity))

    override def ix(i: Unit): AffineTraversal[Option[A], A] = indexOption[A].ix(i)
  }

  implicit final def atSet[A](implicit ev: Ordered[A]): At[Set[A], A, Unit] = new At[Set[A], A, Unit] {
    private def get(i: A)(set: Set[A]): Option[Unit] = if (set.contains(i)) ().some else None

    private def update(i: A): Set[A] => Option[Unit] => Set[A] = set => {
      case Some(_) => set - i
      case None    => set + i
    }

    override def at(i: A): Lens[Set[A], Option[Unit]] = Lens[Set[A], Option[Unit]](get(i))(update(i))

    override def ix(i: A): AffineTraversal[Set[A], Unit] = indexSet[A].ix(i)
  }

  implicit final def atMap[K, V](implicit ev: Order[K]): At[Map[K, V], K, V] = new At[Map[K, V], K, V] {
    override def at(i: K): Lens[Map[K, V], Option[V]] =
      Lens { map: Map[K, V] => map.get(i) }(map => _.fold(map - i)(map.updated(i, _)))

    override def ix(i: K): AffineTraversal[Map[K, V], V] = indexMap[K, V].ix(i)
  }
}
