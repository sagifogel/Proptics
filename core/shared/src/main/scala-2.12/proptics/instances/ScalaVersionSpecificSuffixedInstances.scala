package proptics.instances

import scala.Function.const
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import cats.instances.stream._
import cats.{Alternative, Eq}

import proptics.Prism
import proptics.typeclass.Suffixed

private[instances] trait ScalaVersionSpecificSuffixedInstances {
  implicit final def suffixedStream[A: Eq]: Suffixed[Stream[A], Stream[A]] = new Suffixed[Stream[A], Stream[A]] {
    override def suffix(s: Stream[A]): Prism[Stream[A], Stream[A]] =
      Prism.fromPreview[Stream[A], Stream[A]](streamStripSuffix(s))(_ ++ s)
  }

  private def streamStripSuffix[A: Eq](qs: Stream[A])(xs0: Stream[A]): Option[Stream[A]] = {
    @tailrec
    def drop(ls1: Stream[A], ls2: Stream[A]): Stream[A] = (ls1, ls2) match {
      case (_ #:: ps, _ #:: xs) => drop(ps, xs)
      case (Stream(), xs) => xs
      case (_, Stream()) => Stream()
    }

    def zipWith[B, C](f: A => B => C)(listA: Stream[A], listB: Stream[B]): Stream[C] = {
      @tailrec
      def go(ls1: Stream[A], ls2: Stream[B], result: ListBuffer[C]): ListBuffer[C] = (ls1, ls2) match {
        case (Stream(), _) => result
        case (_, Stream()) => result
        case (x #:: xs, y #:: ys) => go(xs, ys, result += f(x)(y))
      }

      Stream(go(listA, listB, new mutable.ListBuffer[C]): _*)
    }

    @tailrec
    def go(ls1: Stream[A], ls2: Stream[A], zs: Stream[A]): Option[Stream[A]] = (ls1, ls2) match {
      case (_ #:: xs, _ #:: ys) => go(xs, ys, zs)
      case (xs, Stream()) =>
        Alternative[Option].guard(Eq[Stream[A]].eqv(xs, qs)) map const(zipWith(const[A, A])(xs0, zs))
      case (Stream(), _) => None
    }

    val dropped = drop(qs, xs0)

    go(xs0, dropped, dropped)
  }
}
