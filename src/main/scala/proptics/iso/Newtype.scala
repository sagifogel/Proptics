package proptics.iso

import proptics.Iso
import proptics.newtype.Newtype.Aux

object Newtype {
  def newtype[T, S, A, B](ta: Aux[T, A])(sb: Aux[S, B]): Iso[T, S, A, B] = Iso(ta.unwrap _)(sb.wrap)
}
