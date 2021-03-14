package proptics.instances

import scala.collection.immutable.ArraySeq

import proptics.Prism
import proptics.typeclass.Cons

private[instances] trait ScalaVersionSpecificConsInstances {
  implicit final def consLazyList[A]: Cons[LazyList[A], A] = new Cons[LazyList[A], A] {
    override def cons: Prism[LazyList[A], (A, LazyList[A])] =
      Prism.fromPreview[LazyList[A], (A, LazyList[A])](list => list.headOption.map((_, list.tail))) { case (head, tail) =>
        head #:: tail
      }
  }

  implicit final def consArraySeq[A]: Cons[ArraySeq[A], A] = new Cons[ArraySeq[A], A] {
    override def cons: Prism[ArraySeq[A], (A, ArraySeq[A])] =
      Prism.fromPreview[ArraySeq[A], (A, ArraySeq[A])](seq => seq.headOption.map((_, seq.tail))) { case (head, tail) =>
        head +: tail
      }
  }
}
