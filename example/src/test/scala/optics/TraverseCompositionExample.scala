package optics

import proptics.Traversal
import proptics.specs.PropticsSuite
import proptics.std.string._
import proptics.unsafe.string._

class TraverseCompositionExample extends PropticsSuite {
  test("Capitalize the first char of every word") {
    val composed = words compose stringToChars compose Traversal.take[List, Char](1)
    val sentence = "capitalize the first char of every word"

    assertResult(composed.over(_.toUpper)(sentence))("Capitalize The First Char Of Every Word")
  }
}
