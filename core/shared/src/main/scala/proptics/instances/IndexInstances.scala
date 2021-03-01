package proptics.instances

import scala.Function.const
import scala.annotation.{switch, tailrec}
import scala.collection.immutable.{ListMap, SortedMap}
import scala.reflect.ClassTag
import scala.util.Try

import cats.Eq
import cats.arrow.Strong
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._

import proptics.profunctor.Choice
import proptics.{AffineTraversal, At, Index}

trait IndexInstances extends ScalaVersionSpecificIndexInstances {
  def index[S, I, A](i: I)(implicit ev: Index[S, I, A]): AffineTraversal[S, A] = ev.ix(i)

  def fromAt[S, I, A](implicit ev: At[S, I, A]): Index[S, I, A] =
    Index.index[S, I, A](i => ev.ix(i).viewOrModify)(i => s => a => ev.ix(i).set(a)(s))

  implicit final def indexArrow[I: Eq, A]: Index[I => A, I, A] = new Index[I => A, I, A] {
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
      AffineTraversal[Array[A], A] { arr =>
        arr.lift(i).fold(arr.asLeft[A])(_.asRight[Array[A]])
      } { arr => a =>
        Try(arr.updated(i, a)).getOrElse(arr)
      }
  }

  implicit final def indexVector[A]: Index[Vector[A], Int, A] = new Index[Vector[A], Int, A] {
    override def ix(i: Int): AffineTraversal[Vector[A], A] =
      AffineTraversal[Vector[A], A] { arr =>
        arr.lift(i).fold(arr.asLeft[A])(_.asRight[Vector[A]])
      } { arr => a =>
        Try(arr.updated(i, a)).getOrElse(arr)
      }
  }

  implicit final def indexList[A]: Index[List[A], Int, A] = new Index[List[A], Int, A] {
    override def ix(i: Int): AffineTraversal[List[A], A] =
      AffineTraversal[List[A], A] { list =>
        list.lift(i).fold(list.asLeft[A])(_.asRight[List[A]])
      } { list => a =>
        Try(list.updated(i, a)).getOrElse(list)
      }
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

  implicit final def indexSortedMap[K, V]: Index[SortedMap[K, V], K, V] = new Index[SortedMap[K, V], K, V] {
    override def ix(i: K): AffineTraversal[SortedMap[K, V], V] =
      AffineTraversal[SortedMap[K, V], V] { map =>
        map.get(i).fold(map.asLeft[V])(_.asRight[SortedMap[K, V]])
      }(map => map.updated(i, _))
  }

  implicit final def indexMap[K, V]: Index[Map[K, V], K, V] = new Index[Map[K, V], K, V] {
    override def ix(i: K): AffineTraversal[Map[K, V], V] =
      AffineTraversal[Map[K, V], V] { map =>
        map.get(i).fold(map.asLeft[V])(_.asRight[Map[K, V]])
      }(map => map.updated(i, _))
  }

  implicit final def indexChain[A]: Index[Chain[A], Int, A] = new Index[Chain[A], Int, A] {
    override def ix(i: Int): AffineTraversal[Chain[A], A] =
      AffineTraversal[Chain[A], A] { chain =>
        chain.get(i).fold(chain.asLeft[A])(_.asRight[Chain[A]])
      }(chain => updatedChain(i, _, chain))
  }

  implicit final def indexNonEmptyVector[A]: Index[NonEmptyVector[A], Int, A] = new Index[NonEmptyVector[A], Int, A] {
    override def ix(i: Int): AffineTraversal[NonEmptyVector[A], A] =
      AffineTraversal[NonEmptyVector[A], A] { vec =>
        vec.get(i).fold(vec.asLeft[A])(_.asRight[NonEmptyVector[A]])
      }(vec => vec.updated(i, _).getOrElse(vec))
  }

  implicit final def indexNonEmptyList[A]: Index[NonEmptyList[A], Int, A] = new Index[NonEmptyList[A], Int, A] {
    override def ix(i: Int): AffineTraversal[NonEmptyList[A], A] = {
      def updated(i: Int, a: A, nel: NonEmptyList[A]): NonEmptyList[A] = {
        @tailrec
        def go(cur: Int, list: List[A], nel2: NonEmptyList[A]): NonEmptyList[A] =
          list match {
            case head :: tail =>
              if (cur === i) nel2.append(a).concat(tail)
              else go(cur + 1, tail, nel2.append(head))
            case Nil => nel2
          }

        (i: @switch) match {
          case 0 => NonEmptyList(a, nel.tail)
          case idx if idx < nel.length =>
            go(1, nel.tail, NonEmptyList.one(nel.head))
          case _ => nel
        }
      }

      AffineTraversal[NonEmptyList[A], A] { nel =>
        get(i, nel.iterator).fold(nel.asLeft[A])(_.asRight[NonEmptyList[A]])
      }(nel => a => updated(i, a, nel))
    }
  }

  implicit final def indexNonEmptySet[A: Eq]: Index[NonEmptySet[A], A, Unit] = new Index[NonEmptySet[A], A, Unit] {
    override def ix(a: A): AffineTraversal[NonEmptySet[A], Unit] =
      AffineTraversal { set: NonEmptySet[A] =>
        if (set.contains(a)) ().asRight[NonEmptySet[A]] else set.asLeft[Unit]
      } { set: NonEmptySet[A] => const(set) }
  }

  implicit final def indexNonEmptyMap[K, V]: Index[NonEmptyMap[K, V], K, V] = new Index[NonEmptyMap[K, V], K, V] {
    override def ix(i: K): AffineTraversal[NonEmptyMap[K, V], V] =
      AffineTraversal[NonEmptyMap[K, V], V] { map =>
        map(i).fold(map.asLeft[V])(_.asRight[NonEmptyMap[K, V]])
      }(map => v => map.add((i, v)))
  }

  implicit final def indexNonEmptyChain[A]: Index[NonEmptyChain[A], Int, A] = new Index[NonEmptyChain[A], Int, A] {
    def updated(i: Int, a: A, nec: NonEmptyChain[A]): NonEmptyChain[A] =
      (i: @switch) match {
        case 0 => NonEmptyChain.fromChainPrepend(a, nec.tail)
        case idx if idx < nec.length =>
          NonEmptyChain.fromChainPrepend(nec.head, updatedChain(i - 1, a, nec.tail))
        case _ => nec
      }

    override def ix(i: Int): AffineTraversal[NonEmptyChain[A], A] =
      AffineTraversal[NonEmptyChain[A], A] { nec =>
        get(i, nec.iterator).fold(nec.asLeft[A])(_.asRight[NonEmptyChain[A]])
      }(nec => updated(i, _, nec))
  }

  private[IndexInstances] def updatedChain[A](i: Int, a: A, chain: Chain[A]): Chain[A] = {
    @tailrec
    def go(cur: Int, chain1: Chain[A], chain2: Chain[A]): Chain[A] =
      chain1.uncons match {
        case Some((head, tail)) =>
          if (cur === i) chain2.append(a).concat(tail)
          else go(cur + 1, tail, chain2.append(head))
        case None => chain2
      }

    if (i >= 0 && i < chain.length) go(0, chain, Chain.empty)
    else chain
  }

  private[IndexInstances] def get[A](i: Int, collection: Iterator[A]): Option[A] =
    if (i < 0) None
    else {
      val it = collection.drop(i)
      if (it.hasNext) Some(it.next())
      else None
    }
}
