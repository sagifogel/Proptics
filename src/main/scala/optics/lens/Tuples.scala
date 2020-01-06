package optics.lens

import cats.arrow.Strong
import optics.Lens

object Tuples {
  def _1[P[_, _], A, B, C](implicit ev: Strong[P]): Lens[P, (A, C), (B, C), A, B] = Lens(ev.first[A, B, C])

  def fst[P[_, _], A, B, C](implicit ev: Strong[P]): Lens[P, (A, C), (B, C), A, B] = _1

  def _2[P[_, _], A, B, C](implicit ev: Strong[P]): Lens[P, (C, A), (C, B), A, B] = Lens(ev.second[A, B, C])

  def snd[P[_, _], A, B, C](implicit ev: Strong[P]): Lens[P, (C, A), (C, B), A, B] = _2
}