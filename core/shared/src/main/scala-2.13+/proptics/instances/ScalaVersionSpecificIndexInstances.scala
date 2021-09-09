package proptics.instances

import scala.collection.immutable.{ArraySeq, LazyList}

import cats.syntax.either._

import proptics.AffineTraversal
import proptics.typeclass.Index

private[instances] trait ScalaVersionSpecificIndexInstances {
  implicit final def indexLazyList[A]: Index[LazyList[A], Int, A] = new Index[LazyList[A], Int, A] {
    override def ix(i: Int): AffineTraversal[LazyList[A], A] =
      AffineTraversal[LazyList[A], A](list => list.lift(i).fold(list.asLeft[A])(_.asRight[LazyList[A]])) { list => a =>
        Either.catchNonFatal(list.updated(i, a)).toOption.getOrElse(list)
      }
  }

  implicit final def indexArraySeq[A]: Index[ArraySeq[A], Int, A] = new Index[ArraySeq[A], Int, A] {
    override def ix(i: Int): AffineTraversal[ArraySeq[A], A] =
      AffineTraversal[ArraySeq[A], A](arr => arr.lift(i).fold(arr.asLeft[A])(_.asRight[ArraySeq[A]])) { arr => a =>
        Either.catchNonFatal(arr.updated(i, a)).toOption.getOrElse(arr)
      }
  }
}
