package proptics

import cats.arrow.Strong
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.eq._
import cats.{Eq, Id}
import proptics.profunctor.Choice

import scala.Function.const
import scala.reflect.ClassTag

/**
  * [[Index]] provides a [[Traversal]] that can be used to read the value associated with a key in a Map-like container
  */
trait Index[M, A, B] {
  def ix(a: A): AffineTraversal[M, B]
}

abstract class IndexInstances {
  implicit final def indexArr[I: Eq, A]: Index[I => A, I, A] = new Index[I => A, I, A] {
    override def ix(i: I): AffineTraversal[I => A, A] =
      AffineTraversal((f: I => A) => f(i).asRight[I => A])((f: I => A) => (a: A) => (j: I) => if (i === j) a else f(j))
  }

  implicit final def indexOption[A]: Index[Option[A], Unit, A] = new Index[Option[A], Unit, A] {
    override def ix(a: Unit): AffineTraversal[Option[A], A] = new AffineTraversal[Option[A], A] {
      override private[proptics] def apply[P[_, _]](pab: P[A, A])(implicit ev0: Choice[P], ev1: Strong[P]): P[Option[A], Option[A]] = {
        val left = ev0.left[A, A, Unit](pab)

        ev1.dimap[Either[A, Unit], Either[A, Unit], Option[A], Option[A]](left)(_.toLeft(a))(_.fold[Option[A]](_.some, const(None)))
      }

      /** view the focus of an [[AffineTraversal_]] or return the modified source of an [[AffineTraversal_]] */
      override def viewOrModify(s: Option[A]): Either[Option[A], A] = s.asLeft[A]
    }
  }

  implicit final def indexIdentity[A]: Index[Id[A], Unit, A] = new Index[Id[A], Unit, A] {
    override def ix(a: Unit): AffineTraversal[Id[A], A] =
      AffineTraversal { a: A => a.asRight[A] }(const(identity[A]))
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

  implicit final def indexSet[A]: Index[Set[A], A, Unit] = new Index[Set[A], A, Unit] {
    override def ix(a: A): AffineTraversal[Set[A], Unit] =
      AffineTraversal { set: Set[A] =>
        if (set.contains(a)) ().asRight[Set[A]] else (set + a).asLeft[Unit]
      } { set: Set[A] => const(set) }
  }

  implicit final def indexMap[K, V]: Index[Map[K, V], K, V] = new Index[Map[K, V], K, V] {
    override def ix(k: K): AffineTraversal[Map[K, V], V] =
      AffineTraversal { map: Map[K, V] =>
        map.get(k).fold(map.asLeft[V])(_.asRight[Map[K, V]])
      } { map: Map[K, V] => map.updated(k, _) }
  }
}

object Index extends IndexInstances
