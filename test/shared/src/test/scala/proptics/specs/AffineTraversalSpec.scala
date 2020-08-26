package proptics.specs

import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.option._
import proptics.AffineTraversal
import proptics.law.{ATraversalRules, AffineTraversalRules, SetterRules, TraversalRules}
import proptics.specs.Compose._
import spire.std.boolean._

import scala.Function.const

class AffineTraversalSpec extends PropticsSuite {
  val jsonAffineTraversal: AffineTraversal[Json, String] = AffineTraversal[Json, String] {
    case JString(value) => value.asRight[Json]
    case json           => json.asLeft[String]
  }(const(JString))

  val fromOptionJsonAffineTraversal: AffineTraversal[Json, String] =
    AffineTraversal.fromOption[Json, String] {
      case JString(value) => value.some
      case _              => None
    }(const(JString))

  val partialJsonAffineTraversal: AffineTraversal[Json, String] =
    AffineTraversal.fromPartial[Json, String] {
      case JString(value) => value
    }(const(JString))

  checkAll("AffineTraversal fromOption", AffineTraversalRules(fromOptionJsonAffineTraversal))
  checkAll("AffineTraversal fromPartial", AffineTraversalRules(partialJsonAffineTraversal))
  checkAll("AffineTraversal apply", AffineTraversalRules(jsonAffineTraversal))
  checkAll("AffineTraversal asTraversal", TraversalRules(jsonAffineTraversal.asTraversal))

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
    jsonAffineTraversal.contains(jStringContent)(jsonContent) shouldEqual true
    jsonAffineTraversal.contains(jStringContent)(emptyStr) shouldEqual false
  }

  test("notContains") {
    jsonAffineTraversal.notContains(jStringContent)(emptyStr) shouldEqual true
    jsonAffineTraversal.notContains(jStringContent)(jsonContent) shouldEqual false
    jsonAffineTraversal.notContains(jStringContent)(jsonContent) shouldEqual (!jsonAffineTraversal.contains(jStringContent)(jsonContent))
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

  checkAll("compose with Iso", AffineTraversalRules(affineTraversal compose iso))
  checkAll("compose with AnIso", AffineTraversalRules(affineTraversal compose anIso))
  checkAll("compose with Lens", AffineTraversalRules(affineTraversal compose lens))
  checkAll("compose with ALens", AffineTraversalRules(affineTraversal compose aLens))
  checkAll("compose with Prism", AffineTraversalRules(affineTraversal compose affineTraversal))
  checkAll("compose with APrism", AffineTraversalRules(affineTraversal compose aPrism))
  checkAll("compose with AffineTraversal", AffineTraversalRules(affineTraversal compose affineTraversal))
  checkAll("compose with AnAffineTraversal", AffineTraversalRules(affineTraversal compose anAffineTraversal))
  checkAll("compose with Traversal", TraversalRules(affineTraversal compose traversal))
  checkAll("compose with ATraversal", ATraversalRules(affineTraversal compose aTraversal))
  checkAll("compose with Setter", SetterRules(affineTraversal compose setter))

  test("compose with Getter") {
    (affineTraversal compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (affineTraversal compose fold).fold(9) shouldEqual 9
  }
}
