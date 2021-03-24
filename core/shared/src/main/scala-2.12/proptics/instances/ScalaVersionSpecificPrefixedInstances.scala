package proptics.instances

import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import spire.tailrec

import proptics.Prism
import proptics.typeclass.Prefixed

private[instances] trait ScalaVersionSpecificPrefixedInstances {
  implicit final def prefixedStream[A: Eq]: Prefixed[Stream[A], Stream[A]] = new Prefixed[Stream[A], Stream[A]] {
    override def prefixed(s: Stream[A]): Prism[Stream[A], Stream[A]] =
      Prism.fromPreview[Stream[A], Stream[A]](streamStripPrefix(s))(s ++ _)
  }

  @tailrec
  private def streamStripPrefix[A: Eq](ls1: Stream[A])(ls2: Stream[A]): Option[Stream[A]] = (ls1, ls2) match {
    case (Stream(), ys) => ys.some
    case (x #:: xs, y #:: ys) if x === y => streamStripPrefix(xs)(ys)
    case _ => None
  }
}
