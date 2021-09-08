package proptics.specs

import spire.std.boolean._

import proptics.AffineTraversal

trait AffineTraversalCompatSuite extends PropticsSuite {
  val jsonAffineTraversal: AffineTraversal[Json, String]

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
}
