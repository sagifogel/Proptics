package proptics.instances

import cats.Traverse

import proptics.indices.FunctorWithIndex

private[instances] trait ScalaVersionSpecificFunctorWithIndexInstances {
  implicit final val functorWithIndexStream: FunctorWithIndex[Stream, Int] = new FunctorWithIndex[Stream, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: Stream[A]): Stream[B] =
      Traverse[Stream].mapWithIndex(fa)(f)
  }
}
