package proptics.std

import scala.Function.const
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

import cats.instances.list._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Eq}
import spire.tailrec

import proptics.typeclass.Cons
import proptics.{AffineTraversal, Iso, Prism}

trait ListOptics {
  /** a monomorphic [[Iso]] from a list to an array */
  final def listToArray[A: ClassTag]: Iso[List[A], Array[A]] = Iso.iso[List[A], Array[A]](_.toArray)(_.toList)

  /** a monomorphic [[Iso]] from a list to a vector */
  final def listToVector[A]: Iso[List[A], Vector[A]] = Iso.iso[List[A], Vector[A]](_.toVector)(_.toList)

  /** a monomorphic [[Iso]] from a list of chars to a string */
  final val charsToString: Iso[List[Char], String] = Iso.iso[List[Char], String](_.mkString)(_.toList)

  /** a monomorphic [[Iso]] for reversing a list */
  final def reverse[A]: Iso[List[A], List[A]] = Iso.involuted[List[A]](_.reverse)

  /** a monomorphic [[Prism]] to test whether a list is empty */
  final def isEmpty[A]: Prism[List[A], Unit] = Prism.nearly(List.empty[A])(_.isEmpty)

  /** a monomorphic [[Prism]] to select optionally the first element of a list */
  final def head[A](implicit ev: Cons[List[A], A]): AffineTraversal[List[A], A] = ev.headOption

  /** a monomorphic [[Prism]] stripping a prefix from a list when used as a [[proptics.Traversal]], or prepending that prefix when run backwards */
  final def prefixedList[A: Eq](list: List[A]): Prism[List[A], List[A]] =
    Prism.fromPreview[List[A], List[A]](stripPrefix(list))(list ++ _)

  /** a monomorphic [[Prism]] stripping a suffix from a list when used as a [[proptics.Traversal]], or appending that suffix when run backwards */
  final def suffixedList[A: Eq](list: List[A]): Prism[List[A], List[A]] =
    Prism.fromPreview[List[A], List[A]](stripSuffix(list))(_ ++ list)

  @tailrec
  private def stripPrefix[A: Eq](ls1: List[A])(ls2: List[A]): Option[List[A]] = (ls1, ls2) match {
    case (Nil, ys) => ys.some
    case (x :: xs, y :: ys) if x === y => stripPrefix(xs)(ys)
    case _ => None
  }

  private def stripSuffix[A: Eq](qs: List[A])(xs0: List[A]): Option[List[A]] = {
    @tailrec
    def drop(ls1: List[A], ls2: List[A]): List[A] = (ls1, ls2) match {
      case (_ :: ps, _ :: xs) => drop(ps, xs)
      case (Nil, xs) => xs
      case (_, Nil) => Nil
    }

    def zipWith[A, B, C](f: A => B => C)(listA: List[A], listB: List[B]): List[C] = {
      @tailrec
      def go(ls1: List[A], ls2: List[B], result: ListBuffer[C]): ListBuffer[C] = (ls1, ls2) match {
        case (Nil, _) => result
        case (_, Nil) => result
        case (x :: xs, y :: ys) => go(xs, ys, result += f(x)(y))
      }

      go(listA, listB, new mutable.ListBuffer[C]()).toList
    }

    @tailrec
    def go(ls1: List[A], ls2: List[A], zs: List[A]): Option[List[A]] = (ls1, ls2) match {
      case (_ :: xs, _ :: ys) => go(xs, ys, zs)
      case (xs, Nil) =>
        Alternative[Option].guard(xs === qs) map const(zipWith(const[A, A])(xs0, zs))
      case (Nil, _) => None
    }

    val dropped = drop(qs, xs0)

    go(xs0, dropped, dropped)
  }
}
