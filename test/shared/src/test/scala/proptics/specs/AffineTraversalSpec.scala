package proptics.specs

import scala.Function.const

import cats.instances.option._
import cats.syntax.either._
import cats.syntax.option._

import proptics.AffineTraversal
import proptics.law.discipline._
import proptics.specs.compose._

class AffineTraversalSpec extends AffineTraversalCompatSuite {
  val jsonAffineTraversal: AffineTraversal[Json, String] = AffineTraversal[Json, String] {
    case JString(value) => value.asRight[Json]
    case json => json.asLeft[String]
  }(const(JString.apply))

  val fromPreviewJsonAffineTraversal: AffineTraversal[Json, String] =
    AffineTraversal.fromPreview[Json, String] {
      case JString(value) => value.some
      case _ => None
    }(const(JString.apply))

  val partialJsonAffineTraversal: AffineTraversal[Json, String] =
    AffineTraversal.fromPartial[Json, String] { case JString(value) =>
      value
    }(const(JString.apply))

  checkAll("AffineTraversal[Int, Int] id", AffineTraversalTests(AffineTraversal.id[Int]).affineTraversal)
  checkAll("AffineTraversal[Json, String] apply", AffineTraversalTests(jsonAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Json, String] asTraversal", TraversalTests(jsonAffineTraversal.asTraversal).traversal)
  checkAll("AffineTraversal[Json, String] fromPartial", AffineTraversalTests(partialJsonAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Json, String] fromOption", AffineTraversalTests(fromPreviewJsonAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Iso[Int, Int]", AffineTraversalTests(affineTraversal compose iso).affineTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with Iso[Int, Int]", AffineTraversalTests(affineTraversal andThen iso).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with AnIso[Int, Int]", AffineTraversalTests(affineTraversal compose anIso).affineTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with AnIso[Int, Int]", AffineTraversalTests(affineTraversal andThen anIso).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Lens[Int, Int]", AffineTraversalTests(affineTraversal compose lens).affineTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with Lens[Int, Int]", AffineTraversalTests(affineTraversal andThen lens).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with ALens[Int, Int]", AffineTraversalTests(affineTraversal compose aLens).affineTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with ALens[Int, Int]", AffineTraversalTests(affineTraversal andThen aLens).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Prism[Int, Int]", AffineTraversalTests(affineTraversal compose prism).affineTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with Prism[Int, Int]", AffineTraversalTests(affineTraversal andThen prism).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with APrism[Int, Int]", AffineTraversalTests(affineTraversal compose aPrism).affineTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with APrism[Int, Int]", AffineTraversalTests(affineTraversal andThen aPrism).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(affineTraversal compose affineTraversal).affineTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with AffineTraversal[Int, Int]", AffineTraversalTests(affineTraversal andThen affineTraversal).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with AnAffineTraversal[Int, Int]", AffineTraversalTests(affineTraversal compose anAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with AnAffineTraversal[Int, Int]", AffineTraversalTests(affineTraversal andThen anAffineTraversal).affineTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Traversal[Int, Int]", TraversalTests(affineTraversal compose traversal).traversal)
  checkAll("AffineTraversal[Int, Int] andThen with Traversal[Int, Int]", TraversalTests(affineTraversal andThen traversal).traversal)
  checkAll("AffineTraversal[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(affineTraversal compose aTraversal).aTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with ATraversal[Int, Int]", ATraversalTests(affineTraversal andThen aTraversal).aTraversal)
  checkAll("AffineTraversal[Int, Int] compose with Setter[Int, Int]", SetterTests(affineTraversal compose setter).setter)
  checkAll("AffineTraversal[Int, Int] andThen with Setter[Int, Int]", SetterTests(affineTraversal andThen setter).setter)
  checkAll("AffineTraversal[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedTraversalTests(affineTraversal compose indexedLens).indexedTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with IndexedLens[Int, Int, Int]", IndexedTraversalTests(affineTraversal andThen indexedLens).indexedTraversal)
  checkAll("AffineTraversal[Int, Int] andThen with AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(affineTraversal andThen anIndexedLens).indexedTraversal)
  checkAll(
    "AffineTraversal[Int, Int] compose with IndexedTraversal[Int, Int, Int]",
    IndexedTraversalTests(affineTraversal compose indexedTraversal).indexedTraversal
  )
  checkAll(
    "AffineTraversal[Int, Int] andThen with IndexedTraversal[Int, Int, Int]",
    IndexedTraversalTests(affineTraversal andThen indexedTraversal).indexedTraversal
  )
  checkAll("AffineTraversal[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(affineTraversal compose indexedSetter).indexedSetter)
  checkAll("AffineTraversal[Int, Int] andThen with IndexedSetter[Int, Int, Int]", IndexedSetterTests(affineTraversal andThen indexedSetter).indexedSetter)

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

  test("andThen with Getter") {
    (affineTraversal andThen getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (affineTraversal compose fold).fold(9) shouldEqual 9
  }

  test("andThen with Fold") {
    (affineTraversal andThen fold).fold(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = affineTraversal compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter") {
    val composed = affineTraversal andThen indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = affineTraversal compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedFold") {
    val composed = affineTraversal andThen indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
