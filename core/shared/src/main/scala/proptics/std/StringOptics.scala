package proptics.std

import proptics.Iso_
import proptics.std.list._

trait StringOptics {
  final val stringToChars: Iso_[String, String, List[Char], List[Char]] = charsToString.reverse
}
