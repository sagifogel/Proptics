package proptics.std

import scala.Function.const
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.option._
import cats.syntax.eq._
import cats.{Alternative, Eq}
import spire.tailrec

import proptics.{Iso, Prism}

trait ListOptics {
  /** a monomorphic [[Iso]] from a list to an Array */
  final def listToArray[A: ClassTag]: Iso[List[A], Array[A]] = Iso.iso[List[A], Array[A]](_.toArray)(_.toList)

  /** a monomorphic [[Iso]] from a List to a Vector */
  final def listToVector[A]: Iso[List[A], Vector[A]] = Iso.iso[List[A], Vector[A]](_.toVector)(_.toList)

  /** a monomorphic [[Prism]] from a List to a NonEmptyList */
  final def listToNonEmptyList[A]: Prism[List[A], NonEmptyList[A]] =
    Prism.fromPreview[List[A], NonEmptyList[A]](NonEmptyList.fromList)(_.toList)

  /** a monomorphic [[Iso]] from a List[char] to a String */
  final val charsToString: Iso[List[Char], String] = Iso.iso[List[Char], String](_.mkString)(_.toList)

  /** a monomorphic [[Prism]] stripping a suffix from a list when used as a [[proptics.Traversal]], or appending that suffix when run backwards */
  final def suffixedList[A: Eq](list: List[A]): Prism[List[A], List[A]] =
    Prism.fromPreview[List[A], List[A]](stripSuffix(list))(_ ++ list)

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
