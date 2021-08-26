package proptics.instances

import scala.collection.immutable.ArraySeq
import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import proptics.Prism
import proptics.typeclass.Prefixed

import scala.annotation.tailrec

private[instances] trait ScalaVersionSpecificPrefixedInstances {
  implicit final def prefixedLazyList[A: Eq]: Prefixed[LazyList[A], LazyList[A]] = new Prefixed[LazyList[A], LazyList[A]] {
    override def prefixed(s: LazyList[A]): Prism[LazyList[A], LazyList[A]] =
      Prism.fromPreview[LazyList[A], LazyList[A]](lazyListStripPrefix(s))(s ++ _)
  }

  implicit final def prefixedArraySeq[A: Eq]: Prefixed[ArraySeq[A], ArraySeq[A]] = new Prefixed[ArraySeq[A], ArraySeq[A]] {
    override def prefixed(s: ArraySeq[A]): Prism[ArraySeq[A], ArraySeq[A]] =
      Prism.fromPreview[ArraySeq[A], ArraySeq[A]](arraySeqStripPrefix(s))(s ++ _)
  }

  @tailrec
  private def lazyListStripPrefix[A: Eq](ls1: LazyList[A])(ls2: LazyList[A]): Option[LazyList[A]] = (ls1, ls2) match {
    case (LazyList(), ys) => ys.some
    case (x #:: xs, y #:: ys) if x === y => lazyListStripPrefix(xs)(ys)
    case _ => None
  }

  @tailrec
  private def arraySeqStripPrefix[A: Eq](ls1: ArraySeq[A])(ls2: ArraySeq[A]): Option[ArraySeq[A]] = (ls1, ls2) match {
    case (ArraySeq(), ys) => ys.some
    case (x +: xs, y +: ys) if x === y => arraySeqStripPrefix(xs)(ys)
    case _ => None
  }
}
