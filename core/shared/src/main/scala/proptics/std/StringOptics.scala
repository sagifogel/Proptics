package proptics.std

import proptics.std.list._
import proptics.{Iso, Prism}

trait StringOptics {
  /** a monomorphic [[Iso]] from a string to a list of chars */
  final val stringToChars: Iso[String, List[Char]] = charsToString.reverse

  /** a monomorphic [[Prism]] stripping a suffix from a string when used as a [[proptics.Traversal]], or appending that suffix when run backwards */
  final def suffixedString(str: String): Prism[String, String] =
    stringToChars compose suffixedList(str.toList) compose charsToString
}
