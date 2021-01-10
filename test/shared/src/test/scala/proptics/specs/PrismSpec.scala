package proptics.specs
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.option._
import spire.std.boolean._

import proptics.Prism
import proptics.law.discipline._
import proptics.specs.Json._
import proptics.specs.compose._
import proptics.std.list._
import proptics.std.string._

class PrismSpec extends PropticsSuite {
  val emptyStr = ""
  val jNumber: JNumber = JNumber(9d)
  val jsonContent: String = "proptics"
  val jStrEmpty: JString = JString("")
  val jStringContent: JString = JString(jsonContent)
  val jStringContentUppercase: JString = JString(jsonContent.toUpperCase)
  def lengthGreaterThan5(str: String): Boolean = greaterThan5(str.length)
  def lengthGreaterThan10(str: String): Boolean = greaterThan10(str.length)

  val only: Prism[Json, Unit] = Prism.only(jNumber)
  val jsonPrism: Prism[Json, String] =
    Prism[Json, String] {
      case JString(value) => value.asRight[Json]
      case json => json.asLeft[String]
    }(JString)

  val fromOptionJsonPrism: Prism[Json, String] =
    Prism.fromPreview[Json, String] {
      case JString(value) => value.some
      case _ => None
    }(JString)

  val partialJsonPrism: Prism[Json, String] =
    Prism.fromPartial[Json, String] { case JString(value) => value }(JString)

  val nearly: Prism[Json, Unit] = Prism.nearly[Json](jNumber) {
    case JNumber(value) => value > 100
    case _ => false
  }

  checkAll("Prism[Int, Int] id", PrismTests(Prism.id[Int]).prism)
  checkAll("Prism[Json, String] fromOption", PrismTests(fromOptionJsonPrism).prism)
  checkAll("Prism[Json, String] fromPartial", PrismTests(partialJsonPrism).prism)
  checkAll("Prism[Json, String] apply", PrismTests(jsonPrism).prism)
  checkAll("Prism[List[Int], Unit] isEmpty", PrismTests(isEmpty[Int]).prism)
  checkAll("Prism[List[Int], List[Int]] prefixedList", PrismTests(prefixedList[Int](List(1))).prism)
  checkAll("Prism[List[Int], List[Int]] suffixedList", PrismTests(suffixedList(List(1))).prism)
  checkAll("Prism[List[Int], List[Int]] prefixedString", PrismTests(prefixedString("A")).prism)
  checkAll("Prism[List[Int], List[Int]] suffixedString", PrismTests(suffixedString("A")).prism)
  checkAll("Prism[Int, Int] compose with Iso[Int, Int]", PrismTests(prism compose iso).prism)
  checkAll("Prism[Int, Int] compose with AnIso[Int, Int]", PrismTests(prism compose anIso).prism)
  checkAll("Prism[Int, Int] compose with Lens[Int, Int]", AffineTraversalTests(prism compose lens).affineTraversal)
  checkAll("Prism[Int, Int] compose with ALens[Int, Int]", AffineTraversalTests(prism compose aLens).affineTraversal)
  checkAll("Prism[Int, Int] compose with Prism[Int, Int]", PrismTests(prism compose prism).prism)
  checkAll("Prism[Int, Int] compose with APrism[Int, Int]", APrismTests(prism compose aPrism).aPrism)
  checkAll("Prism[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(prism compose affineTraversal).affineTraversal)
  checkAll("Prism[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(prism compose anAffineTraversal).anAffineTraversal)
  checkAll("Prism[Int, Int] compose with Traversal[Int, Int]", TraversalTests(prism compose traversal).traversal)
  checkAll("Prism[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(prism compose aTraversal).aTraversal)
  checkAll("Prism[Int, Int] compose with Setter[Int, Int]", SetterTests(prism compose setter).setter)
  checkAll("Prism[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(prism compose indexedTraversal).indexedTraversal)

  test("viewOrModify") {
    jsonPrism.viewOrModify(jStringContent) shouldEqual jsonContent.asRight[Json]
    jsonPrism.viewOrModify(jNumber) shouldEqual jNumber.asLeft[String]
  }

  test("preview") {
    jsonPrism.preview(jStringContent) shouldEqual jsonContent.some
    jsonPrism.preview(jNumber) shouldEqual None
  }

  test("review") {
    jsonPrism.review(jsonContent) shouldEqual jStringContent
  }

  test("set") {
    jsonPrism.set(jsonContent)(jStrEmpty) shouldEqual jStringContent
  }

  test("setOption") {
    jsonPrism.setOption(jsonContent)(jStrEmpty) shouldEqual jStringContent.some
    jsonPrism.setOption(jsonContent)(jNumber) shouldEqual None
  }

  test("over") {
    jsonPrism.over(_.toUpperCase)(jStringContent) shouldEqual jStringContentUppercase
  }

  test("overOption") {
    jsonPrism.overOption(_.toUpperCase)(jStringContent) shouldEqual jStringContentUppercase.some
    jsonPrism.overOption(_.toUpperCase)(jNumber) shouldEqual None
  }

  test("traverse") {
    jsonPrism.traverse(jStringContent)(_.some) shouldEqual Some(jStringContent)
    jsonPrism.traverse(jStringContent)(_.some) shouldEqual jsonPrism.overF(_.some)(jStringContent)
  }

  test("forall") {
    jsonPrism.forall(lengthGreaterThan5 _)(jStringContent) shouldEqual true
    jsonPrism.forall(lengthGreaterThan10 _)(jStringContent) shouldEqual false
    jsonPrism.forall(lengthGreaterThan5 _)(jNumber) shouldEqual true
    jsonPrism.forall(lengthGreaterThan10 _)(jNumber) shouldEqual true
  }

  test("forall using heyting") {
    jsonPrism.forall(jStringContent)(lengthGreaterThan5) shouldEqual true
    jsonPrism.forall(jStringContent)(lengthGreaterThan10) shouldEqual false
    jsonPrism.forall(jNumber)(lengthGreaterThan5) shouldEqual true
    jsonPrism.forall(jNumber)(lengthGreaterThan10) shouldEqual true
  }

  test("exists") {
    jsonPrism.exists(lengthGreaterThan5)(jStringContent) shouldEqual true
    jsonPrism.exists(lengthGreaterThan10)(jStringContent) shouldEqual false
  }

  test("notExists") {
    jsonPrism.notExists(lengthGreaterThan10)(jStringContent) shouldEqual true
    jsonPrism.notExists(lengthGreaterThan5)(jStringContent) shouldEqual false
    jsonPrism.notExists(lengthGreaterThan5)(jStringContent) shouldEqual
      (!jsonPrism.exists(lengthGreaterThan5)(jStringContent))
  }

  test("contains") {
    jsonPrism.contains(jsonContent)(jStringContent) shouldEqual true
    jsonPrism.contains(emptyStr)(jStringContent) shouldEqual false
  }

  test("notContains") {
    jsonPrism.notContains(emptyStr)(jStringContent) shouldEqual true
    jsonPrism.notContains(jsonContent)(jStringContent) shouldEqual false
    jsonPrism.notContains(jsonContent)(jStringContent) shouldEqual (!jsonPrism.contains(jsonContent)(jStringContent))
  }

  test("isEmpty") {
    jsonPrism.isEmpty(jStringContent) shouldEqual false
    jsonPrism.isEmpty(jNumber) shouldEqual true
  }

  test("nonEmpty") {
    jsonPrism.nonEmpty(jStringContent) shouldEqual true
    jsonPrism.nonEmpty(jNumber) shouldEqual false
    jsonPrism.nonEmpty(jStringContent) shouldEqual (!jsonPrism.isEmpty(jStringContent))
  }

  test("find") {
    jsonPrism.find(lengthGreaterThan5)(jStringContent) shouldEqual jsonContent.some
    jsonPrism.find(lengthGreaterThan10)(jStringContent) shouldEqual None
    jsonPrism.find(lengthGreaterThan5)(jNumber) shouldEqual None
    jsonPrism.find(lengthGreaterThan10)(jNumber) shouldEqual None
  }

  test("nearly") {
    nearly.viewOrModify(JNumber(1000)) shouldEqual ().asRight[Json]
    nearly.viewOrModify(JNumber(1)) shouldEqual JNumber(1).asLeft[Unit]
    nearly.review(()) shouldEqual jNumber
  }

  test("only") {
    only.viewOrModify(jNumber) shouldEqual ().asRight[Json]
    only.viewOrModify(JNumber(1000)) shouldEqual JNumber(1000).asLeft[Unit]
    only.review(()) shouldEqual jNumber
  }

  test("compose with Getter") {
    (prism compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (prism compose fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (prism compose review).review(9) shouldEqual 9
  }
}
