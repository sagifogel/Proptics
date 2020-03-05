package proptics.syntax

import cats.arrow.{Profunctor, Strong}
import proptics.Setter
import proptics.internal.Indexed
import proptics.iso.Newtype.newtype
import proptics.newtype.Newtype
import proptics.newtype.Newtype.Aux

object IndexedSyntax {
  implicit class IndexedOps[P[_, _], I, S, T, R](val indexed: Indexed[P, I, S, T => R]) extends AnyVal {
    def reindexed[J](f: I => J)(implicit ev0: Strong[* => *], ev1: Profunctor[P], ev2: Newtype.Aux[(J, S), (I, S)]): Indexed[P, J, S, T => R] = {
      val firstIndex = ev0.first[I, J, S](f)
      val setter = Setter[P[(I, S), T => R], P[(J, S), T => R], (I, S), (J, S)](is2js => pistr => {
        val aux: Aux[(I, S), (J, S)] = Newtype.newtype(ev2.unwrap)(is2js)

        ev1.lmap(pistr)(js => newtype(ev2)(aux).set(js)(ev0)(js))
      })

      Indexed(setter.over(firstIndex)(indexed.runIndex))
    }
  }

}
