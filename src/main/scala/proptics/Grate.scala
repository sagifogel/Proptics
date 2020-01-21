package proptics

import cats.{Distributive, Functor}
import proptics.profunctor.Closed
import proptics.syntax.FunctionSyntax._

/**
 * <a href="http://r6research.livejournal.com/28050.html">A [[Grate]]</a>
 *
 * @tparam P an evidence of [[Closed]] [[cats.arrow.Profunctor]]
 * @tparam S the source of an [[Grate]]
 * @tparam T the modified source of an [[Grate]]
 * @tparam A the target of an [[Grate]]
 * @tparam B the modified target of an [[Grate]]
 */
abstract class Grate[P[_, _] : Closed, S, T, A, B] extends Optic[P, S, T, A, B] { self =>
}

object Grate {
  private[proptics] def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T])(implicit ev: Closed[P]): Grate[P, S, T, A, B] = new Grate[P, S, T, A, B] {
    override def apply(pab: P[A, B]): P[S, T] = f(pab)
  }

  def apply[P[_, _], S, T, A, B](to: ((S => A) => B) => T)(implicit ev: Closed[P], ev2: DummyImplicit): Grate[P, S, T, A, B] = grate(to)

  def grate[P[_, _], S, T, A, B](to: ((S => A) => B) => T)(implicit ev: Closed[P]): Grate[P, S, T, A, B] =
    Grate((pab: P[A, B]) => {
      ev.dimap[(S => A) => A, (S => A) => B, S, T](ev.closed(pab))(_.`#`)(to)
    })

  def cotraversed[P[_, _] : Closed, F[_], A, B](implicit ev: Distributive[F], ev1: Functor[F[A] => *]): Grate[P, F[A], F[B], A, B] = {
    def cotraverse[G[_]: Functor](f: G[A] => B)(gfa: G[F[A]]): F[B] =
      ev.map(ev.cosequence(gfa))(f)

    Grate.grate[P, F[A], F[B], A, B](cotraverse(_)(identity))
  }
}
