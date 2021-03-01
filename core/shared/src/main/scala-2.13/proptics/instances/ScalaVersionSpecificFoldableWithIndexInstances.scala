package proptics.instances

import cats.Eval

import proptics.indices.FoldableWithIndex

private[instances] trait ScalaVersionSpecificFoldableWithIndexInstances {
  implicit val foldableWithIndexLazyList: FoldableWithIndex[LazyList, Int] = new FoldableWithIndex[LazyList, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: LazyList[A], b: B): B =
      fa.foldLeft((b, 0)) { case ((b, i), a) => (f(b, (a, i)), i + 1) }._1

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: LazyList[A], lb: Eval[B]): Eval[B] = {
      def loop(as: LazyList[A], i: Int): Eval[B] =
        as match {
          case LazyList() => lb
          case x #:: xs => f((x, i), Eval.defer(loop(xs, i + 1)))
        }
      Eval.defer(loop(fa, 0))
    }
  }
}
