package proptics.specs

import cats.instances.option._
import cats.syntax.either._
import cats.syntax.option._

import proptics.Prism
import proptics.law.discipline._
import proptics.specs.Json._
import proptics.specs.compose._

class PrismSpec extends PrismCompatSuite {
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
    }(JString.apply)

  val fromOptionJsonPrism: Prism[Json, String] =
    Prism.fromPreview[Json, String] {
      case JString(value) => value.some
      case _ => None
    }(JString.apply)

  val partialJsonPrism: Prism[Json, String] =
    Prism.fromPartial[Json, String] { case JString(value) => value }(JString.apply)

  val nearly: Prism[Json, Unit] = Prism.nearly[Json](jNumber) {
    case JNumber(value) => value > 100
    case _ => false
  }

  checkAll("Prism[Int, Int] id", PrismTests(Prism.id[Int]).prism)
  checkAll("Prism[Json, String] fromOption", PrismTests(fromOptionJsonPrism).prism)
  checkAll("Prism[Json, String] fromPartial", PrismTests(partialJsonPrism).prism)
  checkAll("Prism[Json, String] apply", PrismTests(jsonPrism).prism)
  checkAll("Prism[Int, Int] compose with Iso[Int, Int]", PrismTests(prism compose iso).prism)
  checkAll("Prism[Int, Int] andThen Iso[Int, Int]", PrismTests(prism andThen iso).prism)
  checkAll("Prism[Int, Int] compose with Lens[Int, Int]", AffineTraversalTests(prism compose lens).affineTraversal)
  checkAll("Prism[Int, Int] andThen with Lens[Int, Int]", AffineTraversalTests(prism andThen lens).affineTraversal)
  checkAll("Prism[Int, Int] compose with AnIso[Int, Int]", PrismTests(prism compose anIso).prism)
  checkAll("Prism[Int, Int] andThen with AnIso[Int, Int]", PrismTests(prism andThen anIso).prism)
  checkAll("Prism[Int, Int] compose with ALens[Int, Int]", AffineTraversalTests(prism compose aLens).affineTraversal)
  checkAll("Prism[Int, Int] andThen with ALens[Int, Int]", AffineTraversalTests(prism andThen aLens).affineTraversal)
  checkAll("Prism[Int, Int] compose with Prism[Int, Int]", PrismTests(prism compose prism).prism)
  checkAll("Prism[Int, Int] andThen with Prism[Int, Int]", PrismTests(prism andThen prism).prism)
  checkAll("Prism[Int, Int] compose with APrism[Int, Int]", APrismTests(prism compose aPrism).aPrism)
  checkAll("Prism[Int, Int] andThen with APrism[Int, Int]", APrismTests(prism andThen aPrism).aPrism)
  checkAll("Prism[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(prism compose affineTraversal).affineTraversal)
  checkAll("Prism[Int, Int] andThen with AffineTraversal[Int, Int]", AffineTraversalTests(prism andThen affineTraversal).affineTraversal)
  checkAll("Prism[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(prism compose anAffineTraversal).anAffineTraversal)
  checkAll("Prism[Int, Int] andThen with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(prism andThen anAffineTraversal).anAffineTraversal)
  checkAll("Prism[Int, Int] compose with Traversal[Int, Int]", TraversalTests(prism compose traversal).traversal)
  checkAll("Prism[Int, Int] andThen with Traversal[Int, Int]", TraversalTests(prism andThen traversal).traversal)
  checkAll("Prism[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(prism compose aTraversal).aTraversal)
  checkAll("Prism[Int, Int] andThen with ATraversal[Int, Int]", ATraversalTests(prism andThen aTraversal).aTraversal)
  checkAll("Prism[Int, Int] compose with Setter[Int, Int]", SetterTests(prism compose setter).setter)
  checkAll("Prism[Int, Int] andThen with Setter[Int, Int]", SetterTests(prism andThen setter).setter)
  checkAll("Prism[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedTraversalTests(prism compose indexedLens).indexedTraversal)
  checkAll("Prism[Int, Int] andThen with IndexedLens[Int, Int, Int]", IndexedTraversalTests(prism andThen indexedLens).indexedTraversal)
  checkAll("Prism[Int, Int] compose with AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(prism compose anIndexedLens).indexedTraversal)
  checkAll("Prism[Int, Int] andThen with AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(prism andThen anIndexedLens).indexedTraversal)
  checkAll("Prism[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(prism compose indexedTraversal).indexedTraversal)
  checkAll("Prism[Int, Int] andThen with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(prism andThen indexedTraversal).indexedTraversal)
  checkAll("Prism[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(prism compose indexedSetter).indexedSetter)
  checkAll("Prism[Int, Int] andThen with IndexedSetter[Int, Int, Int]", IndexedSetterTests(prism andThen indexedSetter).indexedSetter)

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

  test("isEmpty") {
    jsonPrism.isEmpty(jStringContent) shouldEqual false
    jsonPrism.isEmpty(jNumber) shouldEqual true
  }

  test("nonEmpty") {
    jsonPrism.nonEmpty(jStringContent) shouldEqual true
    jsonPrism.nonEmpty(jNumber) shouldEqual false
    jsonPrism.nonEmpty(jStringContent) shouldEqual !jsonPrism.isEmpty(jStringContent)
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

  test("andThen with Getter") {
    (prism andThen getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (prism compose fold).fold(9) shouldEqual 9
  }

  test("andThen with Fold") {
    (prism andThen fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (prism compose review).review(9) shouldEqual 9
  }

  test("andThen with review") {
    (prism andThen review).review(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = prism compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter") {
    val composed = prism andThen indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = prism compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedFold") {
    val composed = prism andThen indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
