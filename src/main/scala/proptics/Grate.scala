package proptics

import cats.{Distributive, Functor}
import proptics.profunctor.Closed
import proptics.rank2types.Rank2TypeGrateLike
import proptics.syntax.FunctionSyntax._

/**
 * <a href="http://r6research.livejournal.com/28050.html">A [[Grate]]</a>
 *
 * @tparam S the source of an [[Grate]]
 * @tparam T the modified source of an [[Grate]]
 * @tparam A the target of an [[Grate]]
 * @tparam B the modified target of an [[Grate]]
 */
abstract class Grate[S, T, A, B] { self =>
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T]
}

object Grate {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeGrateLike[S, T, A, B]): Grate[S, T, A, B] = new Grate[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T] = f(pab)
}

  def apply[S, T, A, B](to: ((S => A) => B) => T): Grate[S, T, A, B] = grate(to)

  def grate[S, T, A, B](to: ((S => A) => B) => T): Grate[S, T, A, B] =
    Grate(new Rank2TypeGrateLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T] =
        ev.dimap[(S => A) => A, (S => A) => B, S, T](ev.closed(pab))(_.`#`)(to)
    })

  def cotraversed[F[_], A, B](implicit ev: Distributive[F], ev1: Functor[F[A] => *]): Grate[F[A], F[B], A, B] = {
    def cotraverse[G[_]: Functor](f: G[A] => B)(gfa: G[F[A]]): F[B] =
      ev.map(ev.cosequence(gfa))(f)

    Grate.grate[F[A], F[B], A, B](cotraverse(_)(identity))
  }
}
