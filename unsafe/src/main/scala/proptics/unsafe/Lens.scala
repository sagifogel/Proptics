package proptics.unsafe

import proptics.{Lens => SafeLens}

object Lens {
  /** create a monomorphic [[proptics.Lens]] using two view functions that accept the same structure,
    * and a setter function, and simultaneously focus on two distinct parts of it
    */
  def apply2[S, A, B](view1: S => A, view2: S => B)(set: (S, A, B) => S): SafeLens[S, (A, B)] =
    SafeLens[S, (A, B)](s => (view1(s), view2(s)))(s => p => set(s, p._1, p._2))

  /** create a monomorphic [[proptics.Lens]] using three view functions that accept the same structure,
    * and a setter function, and simultaneously focus on three distinct parts of it
    */
  def apply3[S, A, B, C](view1: S => A, view2: S => B, view3: S => C)(set: (S, A, B, C) => S): SafeLens[S, (A, B, C)] =
    SafeLens[S, (A, B, C)](s => (view1(s), view2(s), view3(s)))(s => p => set(s, p._1, p._2, p._3))

  /** create a monomorphic [[proptics.Lens]] using four view functions that accept the same structure,
    * and a setter function, and simultaneously focus on four distinct parts of it
    */
  def apply4[S, A, B, C, D](view1: S => A, view2: S => B, view3: S => C, view4: S => D)(set: (S, A, B, C, D) => S): SafeLens[S, (A, B, C, D)] =
    SafeLens[S, (A, B, C, D)](s => (view1(s), view2(s), view3(s), view4(s)))(s => p => set(s, p._1, p._2, p._3, p._4))

  /** create a monomorphic [[proptics.Lens]] using five view functions that accept the same structure,
    * and a setter function, and simultaneously focus on five distinct parts of it
    */
  def apply5[S, A, B, C, D, E](view1: S => A, view2: S => B, view3: S => C, view4: S => D, view5: S => E)(set: (S, A, B, C, D, E) => S): SafeLens[S, (A, B, C, D, E)] =
    SafeLens[S, (A, B, C, D, E)](s => (view1(s), view2(s), view3(s), view4(s), view5(s)))(s => p => set(s, p._1, p._2, p._3, p._4, p._5))
}
