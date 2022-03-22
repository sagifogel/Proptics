package proptics.syntax.applied

import proptics.applied.AppliedTraversal
import proptics.std.string._
import proptics.{AppliedTraversal, Traversal}

trait AppliedStringSyntax {
  implicit final def appliedStringsOps(s: String): AppliedStringsOps = AppliedStringsOps(s)
}

final case class AppliedStringsOps(private val s: String) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def toChars: AppliedTraversal[String, List[Char]] = AppliedTraversal(s, Traversal.id[String].andThen(stringToChars))

  /** fold over the individual words of a String */
  def toWords: AppliedTraversal[String, String] = AppliedTraversal(s, words)
}
