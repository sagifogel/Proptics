package proptics.specs

import cats.data.State

import proptics.specs.compose._
import proptics.{Review, Review_}

class ReviewSpec extends PropticsSuite {
  implicit val state: State[Int, Whole] = State.pure[Int, Whole](whole9)
  val wholeReview: Review[Whole, Int] = Review[Whole, Int](Whole.apply)
  val polymorphicReview: Review_[(Int, Int), (Int, String), Int, Int] =
    Review_[(Int, Int), (Int, String), Int, Int] { i: Int => (i, i.toString) }

  test("review") {
    wholeReview.review(9) shouldEqual whole9
    polymorphicReview.review(9) shouldEqual Tuple2(9, "9")
  }

  test("reuse") {
    wholeReview.reuse.runA(9).value shouldEqual whole9
  }

  test("compose with Iso") {
    (review compose iso).review(9) shouldEqual 9
  }

  test("compose with AnIso") {
    (review compose anIso).review(9) shouldEqual 9
  }

  test("compose with Prism") {
    (review compose prism).review(9) shouldEqual 9
  }

  test("compose with APrism") {
    (review compose aPrism).review(9) shouldEqual 9
  }

  test("compose with Grate") {
    (review compose grate).review(9) shouldEqual 9
  }

  test("compose with Review") {
    (review compose review).review(9) shouldEqual 9
  }
}
