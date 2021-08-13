package proptics.instances

import scala.Function.const
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

import cats.data._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.foldable._
import cats.{Alternative, Eq, Eval, Foldable}

import proptics.Prism
import proptics.std.all.stringToChars
import proptics.std.list._
import proptics.typeclass.Suffixed

trait SuffixedInstances extends ScalaVersionSpecificSuffixedInstances { self =>
  final def suffixed[S, T](s: S)(implicit ev: Suffixed[S, T]): Prism[S, T] = ev.suffixed(s)

  implicit final def suffixedString: Suffixed[String, String] = new Suffixed[String, String] {
    override def suffixed(s: String): Prism[String, String] =
      stringToChars andThen
        self.suffixed[List[Char], List[Char]](s.toList) andThen
        charsToString
  }

  implicit final def suffixedArray[A: Eq: ClassTag]: Suffixed[Array[A], Array[A]] = new Suffixed[Array[A], Array[A]] {
    override def suffixed(s: Array[A]): Prism[Array[A], Array[A]] =
      Prism.fromPreview[Array[A], Array[A]](arr => arrayStripSuffix(s.toList)(arr.toList))(_ ++ s)
  }

  implicit final def suffixedVector[A: Eq]: Suffixed[Vector[A], Vector[A]] = new Suffixed[Vector[A], Vector[A]] {
    override def suffixed(s: Vector[A]): Prism[Vector[A], Vector[A]] =
      Prism.fromPreview[Vector[A], Vector[A]](vectorStripSuffix(s))(_ ++ s)
  }

  implicit final def suffixedList[A: Eq]: Suffixed[List[A], List[A]] = new Suffixed[List[A], List[A]] {
    override def suffixed(s: List[A]): Prism[List[A], List[A]] =
      Prism.fromPreview[List[A], List[A]](listStripSuffix(s))(_ ++ s)
  }

  implicit final def suffixedChain[A: Eq]: Suffixed[Chain[A], Chain[A]] = new Suffixed[Chain[A], Chain[A]] {
    override def suffixed(s: Chain[A]): Prism[Chain[A], Chain[A]] =
      Prism.fromPreview[Chain[A], Chain[A]](chainStripSuffix(s))(_ ++ s)
  }

  implicit final def suffixedNonEmptyVector[A: Eq]: Suffixed[NonEmptyVector[A], Vector[A]] = new Suffixed[NonEmptyVector[A], Vector[A]] {
    override def suffixed(s: NonEmptyVector[A]): Prism[NonEmptyVector[A], Vector[A]] =
      Prism.fromPreview[NonEmptyVector[A], Vector[A]](nel => vectorStripSuffix(s.toVector)(nel.toVector)) { vec =>
        NonEmptyVector.fromVector(vec).fold(s)(_ ++: s)
      }
  }

  implicit final def suffixedNonEmptyList[A: Eq]: Suffixed[NonEmptyList[A], List[A]] = new Suffixed[NonEmptyList[A], List[A]] {
    override def suffixed(s: NonEmptyList[A]): Prism[NonEmptyList[A], List[A]] =
      Prism.fromPreview[NonEmptyList[A], List[A]](nel => listStripSuffix(s.toList)(nel.toList)) { ls =>
        NonEmptyList.fromList(ls).fold(s)(_.concatNel(s))
      }
  }

  implicit final def suffixedNonEmptyChain[A: Eq]: Suffixed[NonEmptyChain[A], Chain[A]] = new Suffixed[NonEmptyChain[A], Chain[A]] {
    override def suffixed(s: NonEmptyChain[A]): Prism[NonEmptyChain[A], Chain[A]] =
      Prism.fromPreview[NonEmptyChain[A], Chain[A]](nec => chainStripSuffix(s.toChain)(nec.toChain)) { chain =>
        NonEmptyChain.fromChain(chain).fold(s)(_ ++ s)
      }
  }

  implicit final def suffixedOneAnd[F[_]: Alternative: Foldable, A: Eq](implicit ev: Suffixed[F[A], F[A]]): Suffixed[OneAnd[F, A], F[A]] = new Suffixed[OneAnd[F, A], F[A]] {
    override def suffixed(s: OneAnd[F, A]): Prism[OneAnd[F, A], F[A]] =
      Prism.fromPreview[OneAnd[F, A], F[A]](oneAnd => ev.suffixed(s.unwrap).preview(oneAnd.unwrap)) { fa =>
        fa.foldRight(Eval.now(s)) { (a, eval) =>
          val alternative = Alternative[F]

          eval.map { oneAnd =>
            val head = alternative.pure(oneAnd.head)
            val combined = alternative.combineK(head, oneAnd.tail)
            OneAnd(a, combined)
          }
        }.value
      }
  }

  private def arrayStripSuffix[A: Eq: ClassTag](qs: List[A])(xs0: List[A]): Option[Array[A]] = {
    @tailrec
    def drop(ls1: List[A], ls2: List[A]): List[A] = (ls1, ls2) match {
      case (_ :: ps, _ :: xs) => drop(ps, xs)
      case (Nil, xs) => xs
      case (_, Nil) => Nil
    }

    def zipWith[B, C](f: A => B => C)(listA: List[A], listB: List[B]): ListBuffer[C] = {
      @tailrec
      def go(ls1: List[A], ls2: List[B], result: ListBuffer[C]): ListBuffer[C] = (ls1, ls2) match {
        case (Nil, _) => result
        case (_, Nil) => result
        case (x :: xs, y :: ys) => go(xs, ys, result += f(x)(y))
      }

      go(listA, listB, new mutable.ListBuffer[C]())
    }

    @tailrec
    def go(ls1: List[A], ls2: List[A], zs: List[A]): Option[ListBuffer[A]] = (ls1, ls2) match {
      case (_ :: xs, _ :: ys) => go(xs, ys, zs)
      case (xs, Nil) =>
        Alternative[Option].guard(xs === qs) map const(zipWith(const[A, A])(xs0, zs))
      case (Nil, _) => None
    }

    val dropped = drop(qs, xs0)

    go(xs0, dropped, dropped).map(_.toArray)
  }

  protected def vectorStripSuffix[A: Eq](qs: Vector[A])(xs0: Vector[A]): Option[Vector[A]] =
    listStripSuffix(qs.toList)(xs0.toList).map(_.toVector)

  private def listStripSuffix[A: Eq](qs: List[A])(xs0: List[A]): Option[List[A]] = {
    @tailrec
    def drop(ls1: List[A], ls2: List[A]): List[A] = (ls1, ls2) match {
      case (_ :: ps, _ :: xs) => drop(ps, xs)
      case (Nil, xs) => xs
      case (_, Nil) => Nil
    }

    def zipWith[B, C](f: A => B => C)(listA: List[A], listB: List[B]): List[C] = {
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

  private def chainStripSuffix[A: Eq](qs: Chain[A])(xs0: Chain[A]): Option[Chain[A]] = {
    @tailrec
    def drop(ch1: Chain[A], ch2: Chain[A]): Chain[A] = (ch1.uncons, ch2.uncons) match {
      case (Some((_, ps)), Some((_, xs))) => drop(ps, xs)
      case (None, xs) => xs.fold(Chain.empty[A]) { case (a, chain) => Chain.one(a) ++ chain }
      case (_, None) => Chain.empty[A]
    }

    def zipWith[B, C](f: A => B => C)(chainA: Chain[A], chainB: Chain[B]): Chain[C] = {
      @tailrec
      def go(ch1: Chain[A], ch2: Chain[B], result: ListBuffer[C]): ListBuffer[C] = (ch1.uncons, ch2.uncons) match {
        case (None, _) => result
        case (_, None) => result
        case (Some((x, xs)), Some((y, ys))) => go(xs, ys, result += f(x)(y))
      }

      Chain.fromSeq(go(chainA, chainB, new mutable.ListBuffer[C]()).toSeq)
    }

    @tailrec
    def go(ch1: Chain[A], ch2: Chain[A], zs: Chain[A]): Option[Chain[A]] = (ch1.uncons, ch2.uncons) match {
      case (Some((_, xs)), Some((_, ys))) => go(xs, ys, zs)
      case (_, None) =>
        Alternative[Option].guard(ch1 === qs) map const(zipWith(const[A, A])(xs0, zs))
      case (None, _) => None
    }

    val dropped = drop(qs, xs0)

    go(xs0, dropped, dropped)
  }
}
