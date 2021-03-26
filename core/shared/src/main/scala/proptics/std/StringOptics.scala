package proptics.std

import proptics.Iso
import proptics.std.list._

trait StringOptics {
  /** a monomorphic [[Iso]] from a string to a list of chars */
  final val stringToChars: Iso[String, List[Char]] = charsToString.reverse
}
