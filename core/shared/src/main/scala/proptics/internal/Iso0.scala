package proptics.internal

private[proptics] trait Iso0[S, T, A, B] extends Traversal0[S, T, A, B] with Getter1[S, A] with Review0[T, B]
