package proptics.internal

private[proptics] trait Lens0[S, T, A, B] extends Traversal0[S, T, A, B] with Getter1[S, A]
