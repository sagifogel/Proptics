package proptics.iso

import proptics.Iso_
import proptics.newtype.Newtype.Aux

object Newtype {
  def newtype[T, S, A, B](ta: Aux[T, A])(sb: Aux[S, B]): Iso_[T, S, A, B] = Iso_(ta.unwrap _)(sb.wrap)
}
