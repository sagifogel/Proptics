package optics.examples

import cats.syntax.option._
import optics._
import proptics.specs.PropticsSuite
import proptics.{Grate, Grate_}

import scala.Function.const

final case class Whole(part: Int) extends AnyVal

class GrateExamples extends PropticsSuite {
  type User = String
  type Password = String

  test("create a Grate to zip two tuples") {
    val grateTuples = Grate[(Int, Int), Int](f => (f(_._1), f(_._2)))

    assertResult((9, 23))(grateTuples.zipWith((1, 20), (8, 3))(_ + _))
  }

  test("pre compose authenticate function with initialization function `toUserPassword`") {
    val authenticate: Option[(User, Password)] => Option[Unit] = {
      case Some(("12345", "password!")) => ().some
      case _ => None
    }

    val toUserPassword: Request => Option[(User, Password)] = {
      case POST("users" :: user :: _, password) => (user, password).some
      case _ => None
    }

    val grateTuples = Grate_[Request, Response, Option[(User, Password)], Option[Unit]] { f =>
      f(toUserPassword).fold[Response](Forbidden)(const(OK))
    }

    val partialOver = grateTuples.over(authenticate)

    assertResult(OK)(partialOver(POST(List("users", "12345"), "password!")))
    assertResult(Forbidden)(partialOver(GET(List("users", "12345"))))
    assertResult(Forbidden)(partialOver(POST(List("users", "12345"), "pass")))
  }
}
