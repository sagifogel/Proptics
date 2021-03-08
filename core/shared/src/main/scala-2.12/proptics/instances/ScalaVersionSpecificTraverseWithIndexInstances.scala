package proptics.instances

import cats.instances.stream._
import cats.{Applicative, Eval}

import proptics.indices.TraverseWithIndex
import proptics.instances.foldableWithIndex._
import proptics.instances.functorWithIndex._

private[instances] trait ScalaVersionSpecificTraverseWithIndexInstances {
  implicit val traverseWithIndexStream: TraverseWithIndex[Stream, Int] = new TraverseWithIndex[Stream, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: Stream[A]): Stream[B] =
      functorWithIndexStream.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: Stream[A], b: B): B =
      foldableWithIndexStream.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: Stream[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexStream.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: Stream[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Stream[B]] =
      catsStdInstancesForStream.traverse(fa)(f)
  }
}
