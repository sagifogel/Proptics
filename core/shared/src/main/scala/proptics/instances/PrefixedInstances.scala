package proptics.instances

import scala.annotation.tailrec
import scala.reflect.ClassTag

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector, OneAnd}
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Eq}

import proptics.Prism
import proptics.std.all.stringToChars
import proptics.std.list._
import proptics.typeclass.Prefixed

trait PrefixedInstances extends ScalaVersionSpecificPrefixedInstances { self =>
  final def prefixed[S, T](s: S)(implicit ev: Prefixed[S, T]): Prism[S, T] = ev.prefix(s)

  implicit final def prefixedString: Prefixed[String, String] = new Prefixed[String, String] {
    override def prefix(s: String): Prism[String, String] =
      stringToChars andThen
        self.prefixed[List[Char], List[Char]](s.toList) andThen
        charsToString
  }

  implicit final def prefixedArray[A: Eq: ClassTag]: Prefixed[Array[A], Array[A]] = new Prefixed[Array[A], Array[A]] {
    override def prefix(s: Array[A]): Prism[Array[A], Array[A]] =
      Prism.fromPreview[Array[A], Array[A]](arr => arrayStripPrefix(s.toSeq)(arr.toSeq))(s ++ _)
  }

  implicit final def prefixedVector[A: Eq]: Prefixed[Vector[A], Vector[A]] = new Prefixed[Vector[A], Vector[A]] {
    override def prefix(s: Vector[A]): Prism[Vector[A], Vector[A]] =
      Prism.fromPreview[Vector[A], Vector[A]](vectorStripPrefix(s))(s ++ _)
  }

  implicit final def prefixedList[A: Eq]: Prefixed[List[A], List[A]] = new Prefixed[List[A], List[A]] {
    override def prefix(s: List[A]): Prism[List[A], List[A]] =
      Prism.fromPreview[List[A], List[A]](listStripPrefix(s))(s ++ _)
  }

  implicit final def prefixedChain[A: Eq]: Prefixed[Chain[A], Chain[A]] = new Prefixed[Chain[A], Chain[A]] {
    override def prefix(s: Chain[A]): Prism[Chain[A], Chain[A]] =
      Prism.fromPreview[Chain[A], Chain[A]](chainStripPrefix(s))(s ++ _)
  }

  implicit final def prefixedNonEmptyVector[A: Eq]: Prefixed[NonEmptyVector[A], Vector[A]] = new Prefixed[NonEmptyVector[A], Vector[A]] {
    override def prefix(s: NonEmptyVector[A]): Prism[NonEmptyVector[A], Vector[A]] =
      Prism.fromPreview[NonEmptyVector[A], Vector[A]](nel => vectorStripPrefix(s.toVector)(nel.toVector))(vec => NonEmptyVector.fromVector(vec).fold(s)(s ++: _))
  }

  implicit final def prefixedNonEmptyList[A: Eq]: Prefixed[NonEmptyList[A], List[A]] = new Prefixed[NonEmptyList[A], List[A]] {
    override def prefix(s: NonEmptyList[A]): Prism[NonEmptyList[A], List[A]] =
      Prism.fromPreview[NonEmptyList[A], List[A]](nel => listStripPrefix(s.toList)(nel.toList))(ls => NonEmptyList.fromList(ls).fold(s)(s.concatNel))
  }

  implicit final def prefixedNonEmptyChain[A: Eq]: Prefixed[NonEmptyChain[A], Chain[A]] = new Prefixed[NonEmptyChain[A], Chain[A]] {
    override def prefix(s: NonEmptyChain[A]): Prism[NonEmptyChain[A], Chain[A]] =
      Prism.fromPreview[NonEmptyChain[A], Chain[A]](nec => chainStripPrefix(s.toChain)(nec.toChain))(chain => NonEmptyChain.fromChain(chain).fold(s)(s ++ _))
  }

  implicit final def prefixedOneAnd[F[_]: Alternative, A: Eq](implicit ev: Prefixed[F[A], F[A]]): Prefixed[OneAnd[F, A], F[A]] = new Prefixed[OneAnd[F, A], F[A]] {
    override def prefix(s: OneAnd[F, A]): Prism[OneAnd[F, A], F[A]] =
      Prism.fromPreview[OneAnd[F, A], F[A]](oneAnd => ev.prefix(s.unwrap).preview(oneAnd.unwrap))(fa => OneAnd(s.head, Alternative[F].combineK(s.tail, fa)))
  }

  @tailrec
  private def arrayStripPrefix[A: Eq: ClassTag](ls1: Seq[A])(ls2: Seq[A]): Option[Array[A]] = (ls1, ls2) match {
    case (Seq(), ys) => ys.toArray.some
    case (x +: xs, y +: ys) if x === y => arrayStripPrefix(xs)(ys)
    case _ => None
  }

  @tailrec
  private def vectorStripPrefix[A: Eq](ls1: Vector[A])(ls2: Vector[A]): Option[Vector[A]] = (ls1, ls2) match {
    case (Vector(), ys) => ys.some
    case (x +: xs, y +: ys) if x === y => vectorStripPrefix(xs)(ys)
    case _ => None
  }

  @tailrec
  private def listStripPrefix[A: Eq](ls1: List[A])(ls2: List[A]): Option[List[A]] = (ls1, ls2) match {
    case (Nil, ys) => ys.some
    case (x :: xs, y :: ys) if x === y => listStripPrefix(xs)(ys)
    case _ => None
  }

  @tailrec
  private def chainStripPrefix[A: Eq](ls1: Chain[A])(ls2: Chain[A]): Option[Chain[A]] = (ls1.uncons, ls2.uncons) match {
    case (None, _) => ls2.some
    case (Some((x, xs)), Some((y, ys))) if x === y => chainStripPrefix(xs)(ys)
    case _ => None
  }
}
