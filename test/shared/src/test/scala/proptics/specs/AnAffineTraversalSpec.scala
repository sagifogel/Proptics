package proptics.specs

import scala.Function.const

import cats.instances.option.catsStdInstancesForOption
import cats.syntax.either._
import cats.syntax.option._

import proptics.AnAffineTraversal
import proptics.internal.Stall
import proptics.law.discipline._
import proptics.specs.compose._

class AnAffineTraversalSpec extends AnAffineTraversalCompatSuite {
  val jsonAnAffineTraversal: AnAffineTraversal[Json, String] = AnAffineTraversal[Json, String] {
    case JString(value) => value.asRight[Json]
    case json => json.asLeft[String]
  }(const(JString.apply))

  val fromOptionJsonAnAffineTraversal: AnAffineTraversal[Json, String] =
    AnAffineTraversal.fromOption[Json, String] {
      case JString(value) => value.some
      case _ => None
    }(const(JString.apply))

  val partialJsonAnAffineTraversal: AnAffineTraversal[Json, String] =
    AnAffineTraversal.fromPartial[Json, String] { case JString(value) =>
      value
    }(const(JString.apply))

  checkAll("AnAffineTraversal[Json, String] fromOption", AnAffineTraversalTests(fromOptionJsonAnAffineTraversal).anAffineTraversal)
  checkAll("AnAffineTraversal[Json, String] fromPartial", AnAffineTraversalTests(partialJsonAnAffineTraversal).anAffineTraversal)
  checkAll("AnAffineTraversal[Json, String] apply", AnAffineTraversalTests(jsonAnAffineTraversal).anAffineTraversal)
  checkAll("AnAffineTraversal[Json, String] asAffineTraversal", AffineTraversalTests(jsonAnAffineTraversal.asAffineTraversal).affineTraversal)
  checkAll("AnAffineTraversal[Int, Int] id", AnAffineTraversalTests(AnAffineTraversal.id[Int]).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with Iso[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose iso).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with Iso[Int, Int]", AnAffineTraversalTests(anAffineTraversal andThen iso).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with AnIso[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose anIso).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with AnIso[Int, Int]", AnAffineTraversalTests(anAffineTraversal andThen anIso).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with Lens[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose lens).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with Lens[Int, Int]", AnAffineTraversalTests(anAffineTraversal andThen lens).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with ALens[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose aLens).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with ALens[Int, Int]", AnAffineTraversalTests(anAffineTraversal andThen aLens).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with Prism[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose prism).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with Prism[Int, Int]", AnAffineTraversalTests(anAffineTraversal andThen prism).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with APrism[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose aPrism).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with APrism[Int, Int]", AnAffineTraversalTests(anAffineTraversal andThen aPrism).anAffineTraversal)
  checkAll(
    "AnAffineTraversal[Int, Int] compose with AffineTraversal[Int, Int]",
    AnAffineTraversalTests(anAffineTraversal compose affineTraversal).anAffineTraversal
  )
  checkAll(
    "AnAffineTraversal[Int, Int] andThen with AffineTraversal[Int, Int]",
    AnAffineTraversalTests(anAffineTraversal andThen affineTraversal).anAffineTraversal
  )
  checkAll(
    "AnAffineTraversal[Int, Int] compose with AnAffineTraversal[Int, Int]",
    AnAffineTraversalTests(anAffineTraversal compose anAffineTraversal).anAffineTraversal
  )
  checkAll(
    "AnAffineTraversal[Int, Int] andThen with AnAffineTraversal[Int, Int]",
    AnAffineTraversalTests(anAffineTraversal andThen anAffineTraversal).anAffineTraversal
  )
  checkAll("AnAffineTraversal[Int, Int] compose with Traversal[Int, Int]", TraversalTests(anAffineTraversal compose traversal).traversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with Traversal[Int, Int]", TraversalTests(anAffineTraversal andThen traversal).traversal)
  checkAll("AnAffineTraversal[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(anAffineTraversal compose aTraversal).aTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with ATraversal[Int, Int]", ATraversalTests(anAffineTraversal andThen aTraversal).aTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with Setter[Int, Int]", SetterTests(anAffineTraversal compose setter).setter)
  checkAll("AnAffineTraversal[Int, Int] andThen with Setter[Int, Int]", SetterTests(anAffineTraversal andThen setter).setter)
  checkAll("AnAffineTraversal[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedTraversalTests(anAffineTraversal compose indexedLens).indexedTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with IndexedLens[Int, Int, Int]", IndexedTraversalTests(anAffineTraversal andThen indexedLens).indexedTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(anAffineTraversal compose anIndexedLens).indexedTraversal)
  checkAll("AnAffineTraversal[Int, Int] andThen with AnIndexedLens[Int, Int, Int]", IndexedTraversalTests(anAffineTraversal andThen anIndexedLens).indexedTraversal)
  checkAll(
    "AnAffineTraversal[Int, Int] compose with IndexedTraversal[Int, Int, Int]",
    IndexedTraversalTests(anAffineTraversal compose indexedTraversal).indexedTraversal
  )
  checkAll(
    "AnAffineTraversal[Int, Int] andThen with IndexedTraversal[Int, Int, Int]",
    IndexedTraversalTests(anAffineTraversal andThen indexedTraversal).indexedTraversal
  )
  checkAll("AnAffineTraversal[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(anAffineTraversal compose indexedSetter).indexedSetter)
  checkAll("AnAffineTraversal[Int, Int] andThen with IndexedSetter[Int, Int, Int]", IndexedSetterTests(anAffineTraversal andThen indexedSetter).indexedSetter)

  test("viewOrModify") {
    jsonAnAffineTraversal.viewOrModify(jStringContent) shouldEqual jsonContent.asRight[Json]
    jsonAnAffineTraversal.viewOrModify(jNumber) shouldEqual jNumber.asLeft[String]
  }

  test("preview") {
    jsonAnAffineTraversal.preview(jStringContent) shouldEqual jsonContent.some
    jsonAnAffineTraversal.preview(jNumber) shouldEqual None
  }

  test("set") {
    jsonAnAffineTraversal.set(jsonContent)(jStrEmpty) shouldEqual jStringContent
  }

  test("setOption") {
    jsonAnAffineTraversal.setOption(jsonContent)(jStrEmpty) shouldEqual jStringContent.some
    jsonAnAffineTraversal.setOption(jsonContent)(jNumber) shouldEqual None
  }

  test("over") {
    jsonAnAffineTraversal.over(_.toUpperCase)(jStringContent) shouldEqual jStringContentUppercase
  }

  test("overOption") {
    jsonAnAffineTraversal.overOption(_.toUpperCase)(jStringContent) shouldEqual jStringContentUppercase.some
    jsonAnAffineTraversal.overOption(_.toUpperCase)(jNumber) shouldEqual None
  }

  test("traverse") {
    jsonAnAffineTraversal.traverse(jStringContent)(_.some) shouldEqual Some(jStringContent)
    jsonAnAffineTraversal.traverse(jStringContent)(_.some) shouldEqual jsonAnAffineTraversal.overF(_.some)(jStringContent)
  }

  test("isEmpty") {
    jsonAnAffineTraversal.isEmpty(jStringContent) shouldEqual false
    jsonAnAffineTraversal.isEmpty(jNumber) shouldEqual true
  }

  test("nonEmpty") {
    jsonAnAffineTraversal.nonEmpty(jStringContent) shouldEqual true
    jsonAnAffineTraversal.nonEmpty(jNumber) shouldEqual false
    jsonAnAffineTraversal.nonEmpty(jStringContent) shouldEqual (!jsonAnAffineTraversal.isEmpty(jStringContent))
  }

  test("find") {
    jsonAnAffineTraversal.find(lengthGreaterThan5)(jStringContent) shouldEqual jsonContent.some
    jsonAnAffineTraversal.find(lengthGreaterThan10)(jStringContent) shouldEqual None
    jsonAnAffineTraversal.find(lengthGreaterThan5)(jNumber) shouldEqual None
    jsonAnAffineTraversal.find(lengthGreaterThan10)(jNumber) shouldEqual None
  }

  test("withAffineTraversal") {
    val stall: Stall[String, String, Json, Json] = jsonAnAffineTraversal.toStall

    stall.viewOrModify(jStringContent) shouldEqual jsonContent.asRight[Json]
    stall.viewOrModify(jNumber) shouldEqual jNumber.asLeft[String]
  }

  test("compose with Getter") {
    (anAffineTraversal compose getter).view(9) shouldEqual 9
  }

  test("andThen with Getter") {
    (anAffineTraversal andThen getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (anAffineTraversal compose fold).fold(9) shouldEqual 9
  }

  test("andThen with Fold") {
    (anAffineTraversal andThen fold).fold(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = anAffineTraversal compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter") {
    val composed = anAffineTraversal andThen indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = anAffineTraversal compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedFold") {
    val composed = anAffineTraversal andThen indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
