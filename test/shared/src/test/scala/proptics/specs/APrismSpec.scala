package proptics.specs
import cats.syntax.either._
import cats.syntax.option._
import spire.std.boolean._

import proptics.APrism
import proptics.internal.Market
import proptics.law.discipline._
import proptics.macros.GAPrism
import proptics.specs.compose._

class APrismSpec extends PropticsSuite {
  val genAPrism: APrism[Json, JString] = GAPrism[Json, JString]
  val jsonPrism: APrism[Json, String] =
    APrism[Json, String] {
      case JString(value) => value.asRight[Json]
      case json => json.asLeft[String]
    }(JString)

  val fromOptionJsonPrism: APrism[Json, String] =
    APrism.fromPreview[Json, String] {
      case JString(value) => value.some
      case _ => None
    }(JString)

  val partialJsonPrism: APrism[Json, String] =
    APrism.fromPartial[Json, String] { case JString(value) =>
      value
    }(JString)

  val emptyStr = ""
  val jNumber: JNumber = JNumber(9d)
  val jsonContent: String = "proptics"
  val jStrEmpty: JString = JString("")
  val jStringContent: JString = JString(jsonContent)
  val jStringContentUppercase: JString = JString(jsonContent.toUpperCase)
  def lengthGreaterThan5(str: String): Boolean = greaterThan5(str.length)
  def lengthGreaterThan10(str: String): Boolean = greaterThan10(str.length)

  checkAll("GAPrism[Json, JString]", APrismTests(genAPrism).aPrism)
  checkAll("APrism[Json, String] fromOption", APrismTests(fromOptionJsonPrism).aPrism)
  checkAll("APrism[Json, String] fromPartial", APrismTests(partialJsonPrism).aPrism)
  checkAll("APrism[Json, String] apply", APrismTests(jsonPrism).aPrism)
  checkAll("APrism[Json, String] asPrism", PrismTests(jsonPrism.asPrism).prism)
  checkAll("APrism[Int, Int] id", APrismTests(APrism.id[Int]).aPrism)
  checkAll("APrism[Int, Int] compose with Iso[Int, Int]", APrismTests(aPrism compose iso).aPrism)
  checkAll("APrism[Int, Int] compose with AnIso[Int, Int]", APrismTests(aPrism compose anIso).aPrism)
  checkAll("APrism[Int, Int] compose with Lens[Int, Int]", AffineTraversalTests(aPrism compose lens).affineTraversal)
  checkAll("APrism[Int, Int] compose with ALens[Int, Int]", AffineTraversalTests(aPrism compose aLens).affineTraversal)
  checkAll("APrism[Int, Int] compose with Prism[Int, Int]", APrismTests(aPrism compose prism).aPrism)
  checkAll("APrism[Int, Int] compose with APrism[Int, Int]", APrismTests(aPrism compose aPrism).aPrism)
  checkAll("APrism[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(aPrism compose affineTraversal).affineTraversal)
  checkAll("APrism[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(aPrism compose anAffineTraversal).anAffineTraversal)
  checkAll("APrism[Int, Int] compose with Traversal[Int, Int]", TraversalTests(aPrism compose traversal).traversal)
  checkAll("APrism[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(aPrism compose aTraversal).aTraversal)
  checkAll("APrism[Int, Int] compose with Setter[Int, Int]", SetterTests(aPrism compose setter).setter)
  checkAll("APrism[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedTraversalTests(aPrism compose indexedLens).indexedTraversal)
  checkAll("APrism[Int, Int] compose with AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(aPrism compose anIndexedLens).indexedTraversal)
  checkAll("APrism[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(aPrism compose indexedTraversal).indexedTraversal)
  checkAll("APrism[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(aPrism compose indexedSetter).indexedSetter)

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

  test("withPrism") {
    val market: Market[String, String, Json, Json] = jsonPrism.toMarket

    market.viewOrModify(jStringContent) shouldEqual jsonContent.asRight[Json]
    market.viewOrModify(jNumber) shouldEqual jNumber.asLeft[String]
  }

  test("compose with Getter") {
    (aPrism compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (aPrism compose fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (aPrism compose review).review(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = aPrism compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = aPrism compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
