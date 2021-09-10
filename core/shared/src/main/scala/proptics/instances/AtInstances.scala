package proptics.instances

import scala.Function.const
import scala.collection.immutable.{ListMap, SortedMap}

import cats.syntax.option._

import proptics.instances.index._
import proptics.typeclass.At
import proptics.{AffineTraversal, Lens}

trait AtInstances {
  final def at[S, I, A](i: I)(implicit ev: At[S, I, A]): Lens[S, Option[A]] = ev.at(i)

  /** remove a value associated with a key in a Map-like container */
  final def remove[S, I, A](i: I)(s: S)(implicit ev: At[S, I, A]): S =
    ev.at(i).set(None)(s)

  implicit final def atOption[A]: At[Option[A], Unit, A] = new At[Option[A], Unit, A] {
    override def at(i: Unit): Lens[Option[A], Option[A]] = Lens[Option[A], Option[A]](identity)(const(identity))

    override def ix(i: Unit): AffineTraversal[Option[A], A] = indexOption[A].ix(i)
  }

  implicit final def atSet[A]: At[Set[A], A, Unit] = new At[Set[A], A, Unit] {
    private def get(i: A)(set: Set[A]): Option[Unit] = if (set.contains(i)) ().some else None

    private def update(i: A): Set[A] => Option[Unit] => Set[A] = set => {
      case Some(_) => set + i
      case None => set - i
    }

    override def at(i: A): Lens[Set[A], Option[Unit]] = Lens[Set[A], Option[Unit]](get(i))(update(i))

    override def ix(i: A): AffineTraversal[Set[A], Unit] = indexSet[A].ix(i)
  }

  implicit final def atSortedMap[K, V]: At[SortedMap[K, V], K, V] = new At[SortedMap[K, V], K, V] {
    override def at(i: K): Lens[SortedMap[K, V], Option[V]] =
      Lens[SortedMap[K, V], Option[V]](_.get(i))(sortedMap => _.fold(sortedMap - i)(v => sortedMap + (i -> v)))

    override def ix(i: K): AffineTraversal[SortedMap[K, V], V] = indexSortedMap[K, V].ix(i)
  }

  implicit final def atListMap[K, V]: At[ListMap[K, V], K, V] = new At[ListMap[K, V], K, V] {
    override def at(i: K): Lens[ListMap[K, V], Option[V]] =
      Lens[ListMap[K, V], Option[V]](_.get(i))(listMap => _.fold(listMap - i)(v => listMap + (i -> v)))

    override def ix(i: K): AffineTraversal[ListMap[K, V], V] = indexListMap[K, V].ix(i)
  }

  implicit final def atMap[K, V]: At[Map[K, V], K, V] = new At[Map[K, V], K, V] {
    override def at(i: K): Lens[Map[K, V], Option[V]] =
      Lens((map: Map[K, V]) => map.get(i))(map => _.fold(map - i)(map.updated(i, _)))

    override def ix(i: K): AffineTraversal[Map[K, V], V] = indexMap[K, V].ix(i)
  }
}
