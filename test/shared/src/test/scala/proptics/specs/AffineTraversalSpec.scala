package proptics.specs
import scala.Function.const

import cats.syntax.either._
import cats.syntax.option._
import spire.std.boolean._

import proptics.AffineTraversal
import proptics.law.discipline._
import proptics.specs.compose._

class AffineTraversalSpec extends PropticsSuite {
  val jsonAffineTraversal: AffineTraversal[Json, String] = AffineTraversal[Json, String] {
    case JString(value) => value.asRight[Json]
    case json => json.asLeft[String]
  }(const(JString))

  val fromPreviewJsonAffineTraversal: AffineTraversal[Json, String] =
    AffineTraversal.fromPreview[Json, String] {
      case JString(value) => value.some
      case _ => None
    }(const(JString))

  val partialJsonAffineTraversal: AffineTraversal[Json, String] =
    AffineTraversal.fromPartial[Json, String] { case JString(value) =>
      value
    }(const(JString))

  checkAll("AffineTraversal[Int, Int] id", AffineTraversalTests(AffineTraversal.id[Int]).affineTraversal)
  checkAll("AffineTraversal[Json, String] apply", AffineTraversalTests(jsonAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Json, String] asTraversal", TraversalTests(jsonAffineTraversal.asTraversal).traversal)
  checkAll("AffineTraversal[Json, String] fromPartial", AffineTraversalTests(partialJsonAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Json, String] fromOption", AffineTraversalTests(fromPreviewJsonAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Iso[Int, Int]", AffineTraversalTests(affineTraversal compose iso).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with AnIso[Int, Int]", AffineTraversalTests(affineTraversal compose anIso).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Lens[Int, Int]", AffineTraversalTests(affineTraversal compose lens).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with ALens[Int, Int]", AffineTraversalTests(affineTraversal compose aLens).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Prism[Int, Int]", AffineTraversalTests(affineTraversal compose prism).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with APrism[Int, Int]", AffineTraversalTests(affineTraversal compose aPrism).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(affineTraversal compose affineTraversal).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with AnAffineTraversal[Int, Int]", AffineTraversalTests(affineTraversal compose anAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Traversal[Int, Int]", TraversalTests(affineTraversal compose traversal).traversal)
  checkAll("AffineTraversal[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(affineTraversal compose aTraversal).aTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Setter[Int, Int]", SetterTests(affineTraversal compose setter).setter)
  checkAll("AffineTraversal[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedTraversalTests(affineTraversal compose indexedLens).indexedTraversal)
  checkAll("AffineTraversal[Int, Int] compose with AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(affineTraversal compose anIndexedLens).indexedTraversal)
  checkAll(
    "AffineTraversal[Int, Int] compose with IndexedTraversal[Int, Int, Int]",
    IndexedTraversalTests(affineTraversal compose indexedTraversal).indexedTraversal
  )
  checkAll("AffineTraversal[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(affineTraversal compose indexedSetter).indexedSetter)

  test("viewOrModify") {
    jsonAffineTraversal.viewOrModify(jStringContent) shouldEqual jsonContent.asRight[Json]
    jsonAffineTraversal.viewOrModify(jNumber) shouldEqual jNumber.asLeft[String]
  }

  test("preview") {
    jsonAffineTraversal.preview(jStringContent) shouldEqual jsonContent.some
    jsonAffineTraversal.preview(jNumber) shouldEqual None
  }

  test("set") {
    jsonAffineTraversal.set(jsonContent)(jStrEmpty) shouldEqual jStringContent
  }

  test("setOption") {
    jsonAffineTraversal.setOption(jsonContent)(jStrEmpty) shouldEqual jStringContent.some
    jsonAffineTraversal.setOption(jsonContent)(jNumber) shouldEqual None
  }

  test("over") {
    jsonAffineTraversal.over(_.toUpperCase)(jStringContent) shouldEqual jStringContentUppercase
  }

  test("overOption") {
    jsonAffineTraversal.overOption(_.toUpperCase)(jStringContent) shouldEqual jStringContentUppercase.some
    jsonAffineTraversal.overOption(_.toUpperCase)(jNumber) shouldEqual None
  }

  test("traverse") {
    jsonAffineTraversal.traverse(jStringContent)(_.some) shouldEqual Some(jStringContent)
    jsonAffineTraversal.traverse(jStringContent)(_.some) shouldEqual jsonAffineTraversal.overF(_.some)(jStringContent)
  }

  test("forall") {
    jsonAffineTraversal.forall(lengthGreaterThan5 _)(jStringContent) shouldEqual true
    jsonAffineTraversal.forall(lengthGreaterThan10 _)(jStringContent) shouldEqual false
    jsonAffineTraversal.forall(lengthGreaterThan5 _)(jNumber) shouldEqual true
    jsonAffineTraversal.forall(lengthGreaterThan10 _)(jNumber) shouldEqual true
  }

  test("forall using heyting") {
    jsonAffineTraversal.forall(jStringContent)(lengthGreaterThan5) shouldEqual true
    jsonAffineTraversal.forall(jStringContent)(lengthGreaterThan10) shouldEqual false
    jsonAffineTraversal.forall(jNumber)(lengthGreaterThan5) shouldEqual true
    jsonAffineTraversal.forall(jNumber)(lengthGreaterThan10) shouldEqual true
  }

  test("exists") {
    jsonAffineTraversal.exists(lengthGreaterThan5)(jStringContent) shouldEqual true
    jsonAffineTraversal.exists(lengthGreaterThan10)(jStringContent) shouldEqual false
  }

  test("notExists") {
    jsonAffineTraversal.notExists(lengthGreaterThan10)(jStringContent) shouldEqual true
    jsonAffineTraversal.notExists(lengthGreaterThan5)(jStringContent) shouldEqual false
    jsonAffineTraversal.notExists(lengthGreaterThan5)(jStringContent) shouldEqual
      (!jsonAffineTraversal.exists(lengthGreaterThan5)(jStringContent))
  }

  test("contains") {
    jsonAffineTraversal.contains(jsonContent)(jStringContent) shouldEqual true
    jsonAffineTraversal.contains(emptyStr)(jStringContent) shouldEqual false
  }

  test("notContains") {
    jsonAffineTraversal.notContains(emptyStr)(jStringContent) shouldEqual true
    jsonAffineTraversal.notContains(jsonContent)(jStringContent) shouldEqual false
    jsonAffineTraversal.notContains(jsonContent)(jStringContent) shouldEqual (!jsonAffineTraversal.contains(jsonContent)(jStringContent))
  }

  test("isEmpty") {
    jsonAffineTraversal.isEmpty(jStringContent) shouldEqual false
    jsonAffineTraversal.isEmpty(jNumber) shouldEqual true
  }

  test("nonEmpty") {
    jsonAffineTraversal.nonEmpty(jStringContent) shouldEqual true
    jsonAffineTraversal.nonEmpty(jNumber) shouldEqual false
    jsonAffineTraversal.nonEmpty(jStringContent) shouldEqual (!jsonAffineTraversal.isEmpty(jStringContent))
  }

  test("find") {
    jsonAffineTraversal.find(lengthGreaterThan5)(jStringContent) shouldEqual jsonContent.some
    jsonAffineTraversal.find(lengthGreaterThan10)(jStringContent) shouldEqual None
    jsonAffineTraversal.find(lengthGreaterThan5)(jNumber) shouldEqual None
    jsonAffineTraversal.find(lengthGreaterThan10)(jNumber) shouldEqual None
  }

  test("compose with Getter") {
    (affineTraversal compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (affineTraversal compose fold).fold(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = affineTraversal compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = affineTraversal compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
