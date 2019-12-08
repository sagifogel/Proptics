package optics

import optics.Optic.Optic_

private[optics] abstract class Optic[P[_, _], S, T, A, B] extends Serializable {
  val pab: P[A, B] => P[S, T]
}

object Optic {
  type Optic_[P[_, _], S, A] = Optic[P, S, S, A, A]

  def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T]): Optic[P, S, T, A, B] = new Optic[P, S, T, A, B] {
    val pab: P[A, B] => P[S, T] = f
  }
}

object Optic_ {
  def apply[P[_, _], S, A](pab: P[A, A] => P[S, S]): Optic_[P, S, A] = Optic(pab)
}