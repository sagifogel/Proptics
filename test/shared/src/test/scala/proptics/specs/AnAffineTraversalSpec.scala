package proptics.specs

import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.option._
import proptics.AnAffineTraversal
import proptics.internal.Stall
import proptics.law._
import proptics.specs.Compose._
import spire.std.boolean._

import scala.Function.const

class AnAffineTraversalSpec extends PropticsSuite {
  val jsonAnAffineTraversal: AnAffineTraversal[Json, String] = AnAffineTraversal[Json, String] {
    case JString(value) => value.asRight[Json]
    case json           => json.asLeft[String]
  }(const(JString))

  val fromOptionJsonAnAffineTraversal: AnAffineTraversal[Json, String] =
    AnAffineTraversal.fromOption[Json, String] {
      case JString(value) => value.some
      case _              => None
    }(const(JString))

  val partialJsonAnAffineTraversal: AnAffineTraversal[Json, String] =
    AnAffineTraversal.fromPartial[Json, String] {
      case JString(value) => value
    }(const(JString))

  checkAll("AnAffineTraversal[Json, String] fromOption", AnAffineTraversalTests(fromOptionJsonAnAffineTraversal).anAffineTraversal)
  checkAll("AnAffineTraversal[Json, String] fromPartial", AnAffineTraversalTests(partialJsonAnAffineTraversal).anAffineTraversal)
  checkAll("AnAffineTraversal[Json, String] apply", AnAffineTraversalTests(jsonAnAffineTraversal).anAffineTraversal)
  checkAll("AnAffineTraversal[Json, String] asAffineTraversal", AffineTraversalTests(jsonAnAffineTraversal.asAffineTraversal).affineTraversal)
  checkAll("AnAffineTraversal[Int, Int] id", AnAffineTraversalTests(AnAffineTraversal.id[Int]).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with Iso[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose iso).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with AnIso[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose anIso).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with Lens[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose lens).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with ALens[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose aLens).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with Prism[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose prism).anAffineTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with APrism[Int, Int]", AnAffineTraversalTests(anAffineTraversal compose aPrism).anAffineTraversal)
  checkAll(
    "AnAffineTraversal[Int, Int] compose with AffineTraversal[Int, Int]",
    AnAffineTraversalTests(anAffineTraversal compose affineTraversal).anAffineTraversal
  )
  checkAll(
    "AnAffineTraversal[Int, Int] compose with AnAffineTraversal[Int, Int]",
    AnAffineTraversalTests(anAffineTraversal compose anAffineTraversal).anAffineTraversal
  )
  checkAll("AnAffineTraversal[Int, Int] compose with Traversal[Int, Int]", TraversalTests(anAffineTraversal compose traversal).traversal)
  checkAll("AnAffineTraversal[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(anAffineTraversal compose aTraversal).aTraversal)
  checkAll("AnAffineTraversal[Int, Int] compose with Setter[Int, Int]", SetterTests(anAffineTraversal compose setter).setter)

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

  test("forall") {
    jsonAnAffineTraversal.forall(lengthGreaterThan5 _)(jStringContent) shouldEqual true
    jsonAnAffineTraversal.forall(lengthGreaterThan10 _)(jStringContent) shouldEqual false
    jsonAnAffineTraversal.forall(lengthGreaterThan5 _)(jNumber) shouldEqual true
    jsonAnAffineTraversal.forall(lengthGreaterThan10 _)(jNumber) shouldEqual true
  }

  test("forall using heyting") {
    jsonAnAffineTraversal.forall(jStringContent)(lengthGreaterThan5) shouldEqual true
    jsonAnAffineTraversal.forall(jStringContent)(lengthGreaterThan10) shouldEqual false
    jsonAnAffineTraversal.forall(jNumber)(lengthGreaterThan5) shouldEqual true
    jsonAnAffineTraversal.forall(jNumber)(lengthGreaterThan10) shouldEqual true
  }

  test("exists") {
    jsonAnAffineTraversal.exists(lengthGreaterThan5)(jStringContent) shouldEqual true
    jsonAnAffineTraversal.exists(lengthGreaterThan10)(jStringContent) shouldEqual false
  }

  test("notExists") {
    jsonAnAffineTraversal.notExists(lengthGreaterThan10)(jStringContent) shouldEqual true
    jsonAnAffineTraversal.notExists(lengthGreaterThan5)(jStringContent) shouldEqual false
    jsonAnAffineTraversal.notExists(lengthGreaterThan5)(jStringContent) shouldEqual
      (!jsonAnAffineTraversal.exists(lengthGreaterThan5)(jStringContent))
  }

  test("contains") {
    jsonAnAffineTraversal.contains(jStringContent)(jsonContent) shouldEqual true
    jsonAnAffineTraversal.contains(jStringContent)(emptyStr) shouldEqual false
  }

  test("notContains") {
    jsonAnAffineTraversal.notContains(jStringContent)(emptyStr) shouldEqual true
    jsonAnAffineTraversal.notContains(jStringContent)(jsonContent) shouldEqual false
    jsonAnAffineTraversal.notContains(jStringContent)(jsonContent) shouldEqual (!jsonAnAffineTraversal.contains(jStringContent)(jsonContent))
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
    val stall = jsonAnAffineTraversal.withAffineTraversal[Stall[String, String, Json, Json]](viewOrModify => setter => Stall(viewOrModify, setter))

    stall.viewOrModify(jStringContent) shouldEqual jsonContent.asRight[Json]
    stall.viewOrModify(jNumber) shouldEqual jNumber.asLeft[String]
  }

  test("compose with Getter") {
    (anAffineTraversal compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (anAffineTraversal compose fold).fold(9) shouldEqual 9
  }
}
