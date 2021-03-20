package proptics.unsafe

import cats.Applicative
import proptics.Traversal_
import proptics.rank2types.LensLike

object Traversal2 {
  /** create a polymorphic [[proptics.Traversal_]] using two view functions that accept the same structure,
   * and a setter function, and simultaneously focus on two distinct parts of it
   */
  def apply[S, T, A, B](view1: S => A, view2: S => A)(set: (B, B, S) => T): Traversal_[S, T, A, B] =
    Traversal_.wander(new LensLike[S, T, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] = s => ev.map2(f(view1(s)), f(view2(s)))(set(_, _, s))
    })
}

object Traversal3 {
  /** create a polymorphic [[proptics.Traversal_]] using three view functions that accept the same structure,
   * and a setter function, and simultaneously focus on three distinct parts of it
   */
  def apply[S, T, A, B](view1: S => A, view2: S => A, view3: S => A)(set: (B, B, B, S) => T): Traversal_[S, T, A, B] =
    Traversal_.wander(new LensLike[S, T, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] = s => ev.map3(f(view1(s)), f(view2(s)), f(view3(s)))(set(_, _, _, s))
    })
}

object Traversal4 {
  /** create a polymorphic [[proptics.Traversal_]] using four view functions that accept the same structure,
   * and a setter function, and simultaneously focus on four distinct parts of it
   */
  def apply[S, T, A, B](view1: S => A, view2: S => A, view3: S => A, view4: S => A)(set: (B, B, B, B, S) => T): Traversal_[S, T, A, B] =
    Traversal_.wander(new LensLike[S, T, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] = s => ev.map4(f(view1(s)), f(view2(s)), f(view3(s)), f(view4(s)))(set(_, _, _, _, s))
    })
}

object Traversal5 {
  /** create a polymorphic [[proptics.Traversal_]] using five view functions that accept the same structure,
   * and a setter function, and simultaneously focus on five distinct parts of it
   */
  def apply[S, T, A, B](view1: S => A, view2: S => A, view3: S => A, view4: S => A, view5: S => A)(set: (B, B, B, B, B, S) => T): Traversal_[S, T, A, B] =
    Traversal_.wander(new LensLike[S, T, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] =
        s => ev.map5(f(view1(s)), f(view2(s)), f(view3(s)), f(view4(s)), f(view5(s)))(set(_, _, _, _, _, s))
    })
}
