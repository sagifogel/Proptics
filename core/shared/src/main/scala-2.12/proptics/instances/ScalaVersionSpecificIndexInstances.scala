package proptics.instances

import cats.syntax.either._

import proptics.{AffineTraversal, Index}

private[instances] trait ScalaVersionSpecificIndexInstances {
  implicit final def indexStream[A]: Index[Stream[A], Int, A] = new Index[Stream[A], Int, A] {
    override def ix(i: Int): AffineTraversal[Stream[A], A] =
      AffineTraversal[Stream[A], A] { list =>
        list.lift(i).fold(list.asLeft[A])(_.asRight[Stream[A]])
      } { stream => a =>
        Either.catchNonFatal(stream.updated(i, a)).toOption.getOrElse(stream)
      }
  }
}
