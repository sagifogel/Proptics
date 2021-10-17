package proptics.syntax

import proptics.Traversal
import proptics.typeclass.Each

trait EachSyntax {
  implicit final def tuple2Ops[A](tuple: (A, A)): EachTuple2Ops[A] = EachTuple2Ops(tuple)

  implicit final def eachListOps[A](list: List[A]): EachListOps[A] = EachListOps(list)

}
case class EachTuple2Ops[A](tuple: (A, A)) extends AnyVal {
  def each(implicit ev: Each[(A, A), A]): Traversal[(A, A), A] = Each[(A, A), A].each
}

case class EachListOps[A](list: List[A]) extends AnyVal {
  def each(implicit ev: Each[List[A], A]): Traversal[List[A], A] = Each[List[A], A].each
}
