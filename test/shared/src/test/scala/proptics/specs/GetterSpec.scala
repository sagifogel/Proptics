package proptics.specs
import cats.syntax.option._
import proptics.Getter
import proptics.specs.compose._

class GetterSpec extends PropticsSuite {
  val wholeGetter: Getter[Whole, Int] = Getter[Whole, Int](_.part)

  test("view") {
    wholeGetter.view(whole9) shouldEqual 9
  }

  test("exists") {
    wholeGetter.exists(greaterThan5)(whole9) shouldEqual true
    wholeGetter.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    wholeGetter.notExists(greaterThan5)(whole9) shouldEqual false
    wholeGetter.notExists(greaterThan10)(whole9) shouldEqual true
    wholeGetter.notExists(greaterThan10)(whole9) shouldEqual !wholeGetter.exists(greaterThan10)(whole9)
  }

  test("contains") {
    wholeGetter.contains(9)(whole9) shouldEqual true
    wholeGetter.contains(10)(whole9) shouldEqual false
  }

  test("notContains") {
    wholeGetter.notContains(9)(whole9) shouldEqual false
    wholeGetter.notContains(10)(whole9) shouldEqual true
    wholeGetter.notContains(10)(whole9) shouldEqual !wholeGetter.contains(10)(whole9)
  }

  test("find") {
    wholeGetter.find(greaterThan5)(whole9) shouldEqual 9.some
    wholeGetter.find(greaterThan10)(whole9) shouldEqual None
  }

  test("use") {
    wholeGetter.use.runA(whole9).value shouldEqual 9
  }

  test("compose with Iso") {
    (getter compose iso).view(9) shouldEqual 9
  }
  test("compose with AnIso") {
    (getter compose anIso).view(9) shouldEqual 9
  }

  test("compose with Lens") {
    (getter compose lens).view(9) shouldEqual 9
  }

  test("compose with ALens") {
    (getter compose aLens).view(9) shouldEqual 9
  }

  test("compose with Prism") {
    (getter compose prism).fold(9) shouldEqual 9
  }

  test("compose with APrism") {
    (getter compose aPrism).fold(9) shouldEqual 9
  }

  test("compose with AffineTraversal") {
    (getter compose affineTraversal).fold(9) shouldEqual 9
  }

  test("compose with AnAffineTraversal") {
    (getter compose anAffineTraversal).fold(9) shouldEqual 9
  }

  test("compose with Traversal") {
    (getter compose traversal).fold(9) shouldEqual 9
  }

  test("compose with ATraversal") {
    (getter compose aTraversal).fold(9) shouldEqual 9
  }

  test("compose with Getter") {
    (getter compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (getter compose fold).fold(9) shouldEqual 9
  }
}