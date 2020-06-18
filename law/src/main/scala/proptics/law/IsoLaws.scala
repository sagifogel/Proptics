package proptics.law

import cats.laws._
import proptics.Iso

final case class IsoLaws[S, A](iso: Iso[S, A]) extends AnyVal {
  def reversibility(s: S): IsEq[S] = (iso.review _ compose iso.view)(s) <-> s
}
