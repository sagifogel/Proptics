package proptics.examples

import cats.syntax.option._

import proptics.macros.GPrism
import proptics.specs.PropticsSuite

class GPrismExample extends PropticsSuite {
  test("using GPrism to generate prisms for sum types") {
    val prism = GPrism[Request, GET]

    assertResult(GET(List("path")).some)(prism.preview(GET(List("path"))))
    assertResult(None)(prism.preview(POST(List("path"), "body")))
  }
}
