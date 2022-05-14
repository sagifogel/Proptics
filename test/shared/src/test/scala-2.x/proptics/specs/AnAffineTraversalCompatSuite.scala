package proptics.specs

import cats.instances.option.catsStdInstancesForOption
import cats.syntax.option._
import cats.syntax.semigroup._
import spire.std.boolean._

import proptics.AnAffineTraversal
import proptics.profunctor.Choice.choiceStar

trait AnAffineTraversalCompatSuite extends PropticsSuite {
  val jsonAnAffineTraversal: AnAffineTraversal[Json, String]

  test("failover") {
    val jsonOption = jsonAnAffineTraversal.failover[Option](_ |+| "C")(JString("AB"))(choiceStar, strongStarTupleOfDisj, catsStdInstancesForOption)
    val jsonOption2 = jsonAnAffineTraversal.failover[Option](_ |+| "C")(JNumber(1))(choiceStar, strongStarTupleOfDisj, catsStdInstancesForOption)

    jsonOption shouldEqual JString("ABC").some
    jsonOption2 shouldEqual None
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
      !jsonAnAffineTraversal.exists(lengthGreaterThan5)(jStringContent)
  }

  test("contains") {
    jsonAnAffineTraversal.contains(jsonContent)(jStringContent) shouldEqual true
    jsonAnAffineTraversal.contains(emptyStr)(jStringContent) shouldEqual false
  }

  test("notContains") {
    jsonAnAffineTraversal.notContains(emptyStr)(jStringContent) shouldEqual true
    jsonAnAffineTraversal.notContains(jsonContent)(jStringContent) shouldEqual false
    jsonAnAffineTraversal.notContains(jsonContent)(jStringContent) shouldEqual !jsonAnAffineTraversal.contains(jsonContent)(jStringContent)
  }

}
