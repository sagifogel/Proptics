package optics

import optics.profunctor.Choice


/**
 * @tparam P an evidence of [[Choice]] [[cats.arrow.Profunctor]]
 * @tparam S the source of a [[Prism]]
 * @tparam T the modified source of a [[Prism]]
 * @tparam A the target of a [[Prism]]
 * @tparam B the modified target of a [[Prism]]
 */
abstract class Prism[P[_, _] : Choice, S, T, A, B] extends Optic[P, S, T, A, B] { self =>
}

object Prism {
  private[optics] def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T])(implicit ev: Choice[P]): Prism[P, S, T, A, B] = new Prism[P, S, T, A, B] {
    override def apply(pab: P[A, B]): P[S, T] = f(pab)
  }

  def apply[P[_, _], S, T, A, B](to: B => T)(from: S => Either[T, A])(implicit ev: Choice[P]): Prism[P, S, T, A, B] =
    prism(to)(from)

  def prism[P[_, _], S, T, A, B](to: B => T)(from: S => Either[T, A])(implicit ev: Choice[P]): Prism[P, S, T, A, B] = {
    Prism(pab => {
      val right = ev.right[T, A, T](ev.rmap(pab)(to))

      ev.dimap(right)(from)(_.fold(identity, identity))
    })
  }
}


