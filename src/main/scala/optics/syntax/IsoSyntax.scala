package optics.syntax

import cats.arrow.Profunctor
import optics.Optic
import optics.internal.{Exchange, Re}

object IsoSyntax {

  implicit class IsoReOps[P[_, _], S, T, A, B](val optic: Optic[Re[P, A, B, *, *], S, T, A, B]) extends AnyVal {
    def re: Optic[P, B, A, T, S] = Optic(optic(Re(identity[P[B, A]])).runRe)
  }

  implicit class IsoOps[P[_, _], S, T, A, B](val optic: Optic[Exchange[A, B, *, *], S, T, A, B]) extends AnyVal {
    def withIso[R](f: (S => A) => (B => T) => R): R = {
      val exchange = optic(Exchange(identity, identity))

      f(exchange.get)(exchange.inverseGet)
    }

    def au[E](f: (B => T) => E => S): E => A = withIso(sa => bt => e => sa(f(bt)(e)))

    def auf[E, R](f: P[R, A] => E => B)(g: P[R, S])(implicit ev: Profunctor[P]): E => T =
      withIso(sa => bt => e => bt(f(ev.rmap(g)(sa))(e)))

    def under(f: T => S): B => A = withIso(sa => bt => sa compose f compose bt)
  }
}
