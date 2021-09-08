package proptics.specs

import spire.std.boolean._

import proptics.APrism

trait APrismCompatSuite extends PropticsSuite {
  val jsonPrism: APrism[Json, String]

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
}
