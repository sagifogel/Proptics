package proptics.instances

import scala.Function.const
import scala.annotation.tailrec
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

import cats.instances.lazyList._
import cats.syntax.eq._
import cats.{Alternative, Eq}

import proptics.Prism
import proptics.typeclass.Suffixed

private[instances] trait ScalaVersionSpecificSuffixedInstances {
  implicit final def suffixedLazyList[A: Eq]: Suffixed[LazyList[A], LazyList[A]] = new Suffixed[LazyList[A], LazyList[A]] {
    override def suffixed(s: LazyList[A]): Prism[LazyList[A], LazyList[A]] =
      Prism.fromPreview[LazyList[A], LazyList[A]](lazyListStripSuffix(s))(_ ++ s)
  }

  implicit final def suffixedArraySeq[A: Eq: ClassTag]: Suffixed[ArraySeq[A], ArraySeq[A]] = new Suffixed[ArraySeq[A], ArraySeq[A]] {
    override def suffixed(s: ArraySeq[A]): Prism[ArraySeq[A], ArraySeq[A]] =
      Prism.fromPreview[ArraySeq[A], ArraySeq[A]](arraySeqStripSuffix(s))(_ ++ s)
  }

  private def lazyListStripSuffix[A: Eq](qs: LazyList[A])(xs0: LazyList[A]): Option[LazyList[A]] = {
    @tailrec
    def drop(ls1: LazyList[A], ls2: LazyList[A]): LazyList[A] = (ls1, ls2) match {
      case (_ #:: ps, _ #:: xs) => drop(ps, xs)
      case (LazyList(), xs) => xs
      case (_, LazyList()) => LazyList()
    }

    def zipWith[B, C](f: A => B => C)(listA: LazyList[A], listB: LazyList[B]): LazyList[C] = {
      @tailrec
      def go(ls1: LazyList[A], ls2: LazyList[B], result: ListBuffer[C]): ListBuffer[C] = (ls1, ls2) match {
        case (LazyList(), _) => result
        case (_, LazyList()) => result
        case (x #:: xs, y #:: ys) => go(xs, ys, result += f(x)(y))
      }

      LazyList.from(go(listA, listB, new mutable.ListBuffer[C]()))
    }

    @tailrec
    def go(ls1: LazyList[A], ls2: LazyList[A], zs: LazyList[A]): Option[LazyList[A]] = (ls1, ls2) match {
      case (_ #:: xs, _ #:: ys) => go(xs, ys, zs)
      case (xs, LazyList()) =>
        Alternative[Option].guard(xs === qs) map const(zipWith(const[A, A])(xs0, zs))
      case (LazyList(), _) => None
    }

    val dropped = drop(qs, xs0)

    go(xs0, dropped, dropped)
  }

  private def arraySeqStripSuffix[A: Eq: ClassTag](qs: ArraySeq[A])(xs0: ArraySeq[A]): Option[ArraySeq[A]] = {
    @tailrec
    def drop(arr1: ArraySeq[A], arr2: ArraySeq[A]): ArraySeq[A] = (arr1, arr2) match {
      case (_ +: ps, _ +: xs) => drop(ps, xs)
      case (ArraySeq(), xs) => xs
      case (_, ArraySeq()) => ArraySeq()
    }

    def zipWith[B, C: ClassTag](f: A => B => C)(arrA: ArraySeq[A], arrB: ArraySeq[B]): ArraySeq[C] = {
      @tailrec
      def go(arr1: ArraySeq[A], arr2: ArraySeq[B], result: ListBuffer[C]): ListBuffer[C] = (arr1, arr2) match {
        case (ArraySeq(), _) => result
        case (_, ArraySeq()) => result
        case (x +: xs, y +: ys) => go(xs, ys, result += f(x)(y))
      }

      ArraySeq.from(go(arrA, arrB, new mutable.ListBuffer[C]()))
    }

    @tailrec
    def go(arr1: ArraySeq[A], arr2: ArraySeq[A], zs: ArraySeq[A]): Option[ArraySeq[A]] = (arr1, arr2) match {
      case (_ +: xs, _ +: ys) => go(xs, ys, zs)
      case (xs, ArraySeq()) =>
        Alternative[Option].guard(xs === qs) map const(zipWith(const[A, A])(xs0, zs))
      case (ArraySeq(), _) => None
    }

    val dropped = drop(qs, xs0)

    go(xs0, dropped, dropped)
  }
}
