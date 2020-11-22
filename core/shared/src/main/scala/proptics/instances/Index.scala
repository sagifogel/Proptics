package proptics.instances

import scala.Function.const
import scala.collection.immutable.{ListMap, SortedMap}
import scala.reflect.ClassTag

import cats.Eq
import cats.arrow.Strong
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._

import proptics.profunctor.Choice
import proptics.{AffineTraversal, At, Index}

trait IndexInstances {
  def index[S, I, A](i: I)(implicit ev: Index[S, I, A]): AffineTraversal[S, A] = ev.ix(i)

  def fromAt[S, I, A](implicit ev: At[S, I, A]): Index[S, I, A] =
    Index.index[S, I, A](i => ev.ix(i).viewOrModify)(i => s => a => ev.ix(i).set(a)(s))

  implicit final def indexArr[I: Eq, A]: Index[I => A, I, A] = new Index[I => A, I, A] {
    override def ix(i: I): AffineTraversal[I => A, A] =
      AffineTraversal((f: I => A) => f(i).asRight[I => A])((f: I => A) => (a: A) => (j: I) => if (i === j) a else f(j))
  }

  implicit final def indexOption[A]: Index[Option[A], Unit, A] = new Index[Option[A], Unit, A] {
    override def ix(i: Unit): AffineTraversal[Option[A], A] = new AffineTraversal[Option[A], A] {
      override private[proptics] def apply[P[_, _]](pab: P[A, A])(implicit ev0: Choice[P], ev1: Strong[P]): P[Option[A], Option[A]] = {
        val left = ev0.left[A, A, Unit](pab)

        ev1.dimap[Either[A, Unit], Either[A, Unit], Option[A], Option[A]](left)(_.toLeft(i))(_.fold[Option[A]](_.some, const(None)))
      }

      /** view the focus of an [[AffineTraversal]] or return the modified source of an [[AffineTraversal]] */
      override def viewOrModify(s: Option[A]): Either[Option[A], A] = s.asLeft[A]
    }
  }

  implicit final def indexArray[A: ClassTag]: Index[Array[A], Int, A] = new Index[Array[A], Int, A] {
    override def ix(i: Int): AffineTraversal[Array[A], A] =
      AffineTraversal { arr: Array[A] =>
        arr.lift(i).fold(arr.asLeft[A])(_.asRight[Array[A]])
      } { arr: Array[A] => a =>
        Either.catchNonFatal(arr.updated(i, a)).toOption.getOrElse(arr)
      }
  }

  implicit final def indexList[A]: Index[List[A], Int, A] = new Index[List[A], Int, A] {
    override def ix(i: Int): AffineTraversal[List[A], A] =
      AffineTraversal { list: List[A] =>
        list.lift(i).fold(list.asLeft[A])(_.asRight[List[A]])
      } { list: List[A] => a =>
        Either.catchNonFatal(list.updated(i, a)).toOption.getOrElse(list)
      }
  }

  implicit final def indexSortedMap[K, V]: Index[SortedMap[K, V], K, V] = new Index[SortedMap[K, V], K, V] {
    override def ix(i: K): AffineTraversal[SortedMap[K, V], V] =
      AffineTraversal[SortedMap[K, V], V] { map =>
        map.get(i).fold(map.asLeft[V])(_.asRight[SortedMap[K, V]])
      }(map => map.updated(i, _))
  }

  implicit final def indexListMap[K, V]: Index[ListMap[K, V], K, V] = new Index[ListMap[K, V], K, V] {
    override def ix(i: K): AffineTraversal[ListMap[K, V], V] =
      AffineTraversal[ListMap[K, V], V] { map =>
        map.get(i).fold(map.asLeft[V])(_.asRight[ListMap[K, V]])
      }(map => map.updated(i, _))
  }

  implicit final def indexSet[A]: Index[Set[A], A, Unit] = new Index[Set[A], A, Unit] {
    override def ix(i: A): AffineTraversal[Set[A], Unit] =
      AffineTraversal { set: Set[A] =>
        if (set.contains(i)) ().asRight[Set[A]] else set.asLeft[Unit]
      } { set: Set[A] => const(set) }
  }

  implicit final def indexMap[K, V]: Index[Map[K, V], K, V] = new Index[Map[K, V], K, V] {
    override def ix(i: K): AffineTraversal[Map[K, V], V] =
      AffineTraversal[Map[K, V], V] { map =>
        map.get(i).fold(map.asLeft[V])(_.asRight[Map[K, V]])
      }(map => map.updated(i, _))
  }
}
