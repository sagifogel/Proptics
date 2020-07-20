package proptics.specs

import proptics.{Review, Review_}

class ReviewSpec extends PropticsSuite {
  val review: Review[Whole, Int] = Review[Whole, Int](Whole.apply)
  val polymorphicReview: Review_[(Int, Int), (Int, String), Int, Int] =
    Review_[(Int, Int), (Int, String), Int, Int] { i: Int => (i, i.toString) }

  test("review") {
    review.review(9) shouldEqual whole9
    polymorphicReview.review(9) shouldEqual Tuple2(9, "9")
  }
}
