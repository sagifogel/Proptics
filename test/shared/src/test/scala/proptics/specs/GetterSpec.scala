package proptics.specs

import cats.instances.int._
import cats.syntax.option._
import proptics.Getter

class GetterSpec extends PropticsSuite {
  val getter: Getter[Whole, Int] = Getter[Whole, Int](_.focus)

  test("view") {
    getter.view(whole9) shouldEqual 9
  }

  test("exists") {
    getter.exists(greaterThan5)(whole9) shouldEqual true
    getter.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    getter.notExists(greaterThan5)(whole9) shouldEqual false
    getter.notExists(greaterThan10)(whole9) shouldEqual true
    getter.notExists(greaterThan10)(whole9) shouldEqual !getter.exists(greaterThan10)(whole9)
  }

  test("contains") {
    getter.contains(whole9)(9) shouldEqual true
    getter.contains(whole9)(10) shouldEqual false
  }

  test("notContains") {
    getter.notContains(whole9)(9) shouldEqual false
    getter.notContains(whole9)(10) shouldEqual true
    getter.notContains(whole9)(10) shouldEqual !getter.contains(whole9)(10)
  }

  test("find") {
    getter.find(greaterThan5)(whole9) shouldEqual 9.some
    getter.find(greaterThan10)(whole9) shouldEqual None
  }

  test("use") {
    getter.use.runA(whole9).value shouldEqual 9
  }
}
