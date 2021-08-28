package optics.examples

import cats.Eq
import cats.syntax.either._
import proptics.specs.PropticsSuite
import proptics.std.either._
import proptics.{Prism, Review}

import java.awt.Color

class ReviewExamples extends PropticsSuite {
  implicit val eqColor: Eq[Color] = Eq.fromUniversalEquals[Color]
  implicit val eqFill: Eq[Fill] = Eq.instance[Fill]((color1, color2) =>
    (color1, color2) match {
      case (Solid(c1), Solid(c2)) => c1 === c2
      case (LinearGradient(lc1, lc2), LinearGradient(rc1, rc2)) => lc1 === rc1 && lc2 === rc2
      case (RadialGradient(lc1, lc2, lc3), RadialGradient(rc1, rc2, rc3)) => lc1 === rc1 && lc2 === rc2 && lc3 === rc3
      case _ => false
    })

  test("lifting a value into context") {
    assertResult(9.asRight[String])(right[String, Int].review(9))
  }

  test("extract a value from Prism.only") {
    val prism = Prism.only[Color](Color.white)

    assertResult(Color.white)(prism.review(()))
  }

  test("review will use both constructors of composed optics") {
    val eitherFill = right[String, Fill] andThen Prism.only[Fill](Solid(Color.white))

    assertResult(Right(Solid(Color.white)))(eitherFill.review(()))
  }

  test("using Review optic explicitly") {
    val eitherFillReview =
      Review[Either[String, Fill], Fill](_.asRight[String]) andThen
        Prism.only[Fill](Solid(Color.white))

    assertResult(Right(Solid(Color.white)))(eitherFillReview.review(()))
  }
}
