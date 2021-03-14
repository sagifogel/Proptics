package proptics.instances

import scala.collection.immutable.ListMap

import cats.data._
import cats.instances.option._
import cats.syntax.foldable._
import cats.{Eval, Foldable}

import proptics.indices.FoldableWithIndex
import proptics.instances.FoldableWithIndexInstances._

trait FoldableWithIndexInstances extends ScalaVersionSpecificFoldableWithIndexInstances {
  implicit final val foldableWithIndexOption: FoldableWithIndex[Option, Unit] = new FoldableWithIndex[Option, Unit] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Unit)) => B)(fa: Option[A], b: B): B =
      fa.foldLeft(b)((b, a) => f(b, (a, ())))

    override def foldRightWithIndex[A, B](f: ((A, Unit), Eval[B]) => Eval[B])(fa: Option[A], lb: Eval[B]): Eval[B] =
      catsStdInstancesForOption.foldRight(fa, lb)((a, b) => f((a, ()), b))
  }

  implicit final val foldableWithIndexVector: FoldableWithIndex[Vector, Int] = new FoldableWithIndex[Vector, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: Vector[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: Vector[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)
  }

  implicit final val foldableWithIndexList: FoldableWithIndex[List, Int] = new FoldableWithIndex[List, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: List[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: List[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)
  }

  implicit final def foldableWithIndexListMap[K]: FoldableWithIndex[ListMap[K, *], K] = new FoldableWithIndex[ListMap[K, *], K] {
    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: ListMap[K, A], b: B): B =
      mapLikeFoldLeftWithIndex(fa.foldLeft[B] _)(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: ListMap[K, A], lb: Eval[B]): Eval[B] =
      mapLikeFoldRightWithIndex(f)(fa.toList, lb)
  }

  implicit final def foldableWithIndexMap[K]: FoldableWithIndex[Map[K, *], K] = new FoldableWithIndex[Map[K, *], K] {
    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: Map[K, A], b: B): B =
      mapLikeFoldLeftWithIndex(fa.foldLeft[B] _)(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: Map[K, A], lb: Eval[B]): Eval[B] =
      mapLikeFoldRightWithIndex(f)(fa.toList, lb)
  }

  implicit final val foldableWithIndexChain: FoldableWithIndex[Chain, Int] = new FoldableWithIndex[Chain, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: Chain[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: Chain[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)
  }

  implicit final val foldableWithIndexNonEmptyVector: FoldableWithIndex[NonEmptyVector, Int] = new FoldableWithIndex[NonEmptyVector, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyVector[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyVector[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)
  }

  implicit final val foldableWithIndexNonEmptyList: FoldableWithIndex[NonEmptyList, Int] = new FoldableWithIndex[NonEmptyList, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyList[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyList[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)
  }

  implicit final val foldableWithIndexNonEmptyChain: FoldableWithIndex[NonEmptyChain, Int] = new FoldableWithIndex[NonEmptyChain, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyChain[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.toChain.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyChain[A], lb: Eval[B]): Eval[B] =
      listFoldRightWithIndex(f)(fa.toList, lb)
  }

  implicit final def foldableWithIndexOneAnd[F[_]: Foldable]: FoldableWithIndex[OneAnd[F, *], Int] = new FoldableWithIndex[OneAnd[F, *], Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: OneAnd[F, A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: OneAnd[F, A], lb: Eval[B]): Eval[B] =
      listFoldRightWithIndex(f)(fa.toList, lb)
  }

  implicit final def foldableWithIndexNonEmptyMap[K]: FoldableWithIndex[NonEmptyMap[K, *], K] = new FoldableWithIndex[NonEmptyMap[K, *], K] {
    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: NonEmptyMap[K, A], b: B): B =
      mapLikeFoldLeftWithIndex(fa.toSortedMap.foldLeft[B] _)(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: NonEmptyMap[K, A], lb: Eval[B]): Eval[B] =
      mapLikeFoldRightWithIndex(f)(fa.toSortedMap.toList, lb)
  }
}

private[instances] object FoldableWithIndexInstances {
  private[instances] def mapLikeFoldRightWithIndex[A, B, K](f: ((A, K), Eval[B]) => Eval[B])(list: List[(K, A)], lb: Eval[B]): Eval[B] = {
    def loop(ls: List[(K, A)]): Eval[B] =
      Eval.defer(ls match {
        case (k, a) :: xs => f((a, k), loop(xs))
        case Nil => lb
      })

    Eval.always(list).flatMap(loop)
  }

  private[instances] def listFoldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(list: List[A], lb: Eval[B]): Eval[B] = {
    def loop(ls: List[A], i: Int): Eval[B] = ls match {
      case x :: xs => f((x, i), Eval.defer(loop(xs, i + 1)))
      case Nil => lb
    }

    Eval.defer(loop(list, 0))
  }

  private[instances] def listLikeFoldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(itr: Iterator[A], lb: Eval[B]): Eval[B] = {
    def loop(ls: Iterator[A], i: Int): Eval[B] =
      if (ls.hasNext) f((ls.next(), i), Eval.defer(loop(ls, i + 1)))
      else lb

    Eval.defer(loop(itr, 0))
  }

  private[instances] def listLikeFoldLeftWithIndex[A, B](foldLeft: ((B, Int)) => (((B, Int), A) => (B, Int)) => (B, Int))(f: (B, (A, Int)) => B)(b: B): B =
    foldLeft((b, 0)) { case ((b, i), a) => (f(b, (a, i)), i + 1) }._1

  private[instances] def mapLikeFoldLeftWithIndex[A, B, K](foldLeft: B => ((B, (K, A)) => B) => B)(f: (B, (A, K)) => B)(b: B): B =
    foldLeft(b)((b, pair) => f(b, pair.swap))
}
