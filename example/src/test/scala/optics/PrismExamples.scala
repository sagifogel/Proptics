package optics

import scala.Function.const

import cats.Eq
import cats.instances.list._
import cats.instances.option._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.semigroup._

import proptics.instances.cons._
import proptics.specs.PropticsSuite
import proptics.std.either._
import proptics.std.list.{isEmpty, _}
import proptics.std.option._
import proptics.std.string._
import proptics.std.tuple._
import proptics.syntax.function._
import proptics.{Lens, _}

class PrismExamples extends PropticsSuite {
  implicit val eqRequest: Eq[Request] = Eq.instance[Request]((req1, req2) =>
    (req1, req2) match {
      case (GET(path1), GET(path2)) => path1 === path2
      case (DELETE(path1), DELETE(path2)) => path1 === path2
      case (POST(path1, body1), POST(path2, body2)) => path1 === path2 && body1 === body2
      case _ => false
    })
  val get: Prism[Request, Path] = Prism.fromPartial[Request, Path] { case GET(path) => path }(GET)
  val delete: Prism[Request, Path] = Prism.fromPartial[Request, Path] { case DELETE(path) => path }(DELETE)
  val post: Prism[Request, (Path, Body)] =
    Prism.fromPartial[Request, (Path, Body)] { case POST(path, body) => (path, body) } { case (path, body) =>
      POST(path, body)
    }

  val path: Lens[Request, Path] = Lens[Request, Path] {
    case GET(path) => path
    case POST(path, _) => path
    case DELETE(path) => path
  } {
    case GET(_) => GET
    case post: POST => path => post.copy(path = path)
    case DELETE(_) => DELETE
  }

  val serveRequest: Request => String = const("404 Not Found")
  val startsWith: String => AffineTraversal[Request, Unit] = path compose head[String] compose Prism.only[String](_)
  val pathPrefix: String => Prism[Request, Request] = prefix =>
    Prism.fromPartial[Request, Request] {
      case req: Request if startsWith(prefix).nonEmpty(req) => path.over(_.drop(1))(req)
    }(path.over(prefix :: _))

  val userHandler: Request => String =
    req => "User handler! Remaining path: " |+| path.view(req).intercalate("/")

  test("manipulate an Either") {
    assertResult(Some("message"))(left[String, Int].preview(Left("message")))
    assertResult(None)(left[String, Int].preview(Right(9)))
    assertResult(Right(9))(right[String, Int].over(_ + 1)(Right(8)))
    assertResult(Left("message"))(left[String, Int].over(_.reverse)(Left("egassem")))
    assertResult(true)(Fold.has(left[String, Int])(Left("message")))
    assertResult(false)(Fold.has(right[String, Int])(Left("message")))
  }

  test("representation of HTTP web requests as a sum type") {
    val traversal = post compose _1[Path, Body]
    val expected = POST(List("users", "12345"), "name: John")

    assertResult(List("users").some)(GET(List("users")) & get.preview)
    assertResult(None)(GET(List("users")) & post.preview)
    assertResult(GET(List("posts")))(GET(List("users")) & get.set(List("posts")))
    assertResult((List("users"), "name: John").some)(post.preview(POST(List("users"), "name: John")))
    assertResult(expected)(POST(List("users"), "name: John") & traversal.over(_ |+| List("12345")))
  }

  test("embedding values with prisms") {
    val composed: Prism[Option[Either[String, Int]], String] =
      some[Either[String, Int]] compose left[String, Int]

    assertResult(GET(List("users")))(get.review(List("users")))
    assertResult(POST(List("users"), "My blog post"))(post.review((List("users"), "My blog post")))
    assertResult(Left("an error"))(left[String, Int].review("an error"))
    assertResult(Some(Left("value")))(composed.review("value"))
  }

  test("extract all Some values from an list") {
    val input = List("Some".some, "None".some, None, "Option".some, None)
    val composed = Fold.fromFoldable[List, Option[String]] compose some[String]

    assertResult(List("Some", "None", "Option"))(composed.viewAll(input))
  }

  test("traverse all options values") {
    val input = List("Some".some, None, "Option".some)
    val composed = Traversal.fromTraverse[List, Option[String]] compose some[String]
    val expected = List("Some[A]".some, None, "Option[A]".some)

    assertResult(expected)(composed.over(_ |+| "[A]")(input))
  }

  test("using outside the create safeTail") {
    val outside = Lens.outside[List[Int], Unit, List[Int]](isEmpty)
    val safeTail: List[Int] => List[Int] = outside.set(const(List.empty[Int]))(_.tail)

    assertResult(List.empty[Int])(safeTail(List.empty[Int]))
  }

  test("check whether a list is empty using nearly") {
    val isEmpty = Prism.nearly(List.empty[String])(_.isEmpty)

    assertResult(().some)(isEmpty.preview(List.empty[String]))
    assertResult(None)(isEmpty.preview(List("contains", "values")))
  }

  test("check whether a an option contains a specific value") {
    val hasProptics = Prism.only[Option[String]]("Proptics".some)

    assertResult(None)(hasProptics.preview("other".some))
    assertResult(None)(hasProptics.preview(None))
    assertResult(Some(()))(hasProptics.preview("Proptics".some))
  }

  test("is secured request") {
    val secure = prefixedString("https://")
    assertResult(None)(secure.preview("http://sagifogel.github.io/Proptics/"))
    assertResult("sagifogel.github.io/Proptics/".some)(secure.preview("https://sagifogel.github.io/Proptics/"))
  }

  test("creating the user route") {
    val server =
      serveRequest & Lens.outside(pathPrefix("users")).set(userHandler)

    assertResult("User handler! Remaining path: 12345")(server(GET(List("users", "12345"))))
  }

  test("adding simple posts handler") {
    val server =
      serveRequest &
        Lens.outside(pathPrefix("users")).set(userHandler) &
        Lens.outside(pathPrefix("posts")).set(const("Posts Handler!"))

    assertResult("Posts Handler!")(server(POST(List("posts"), "My new post")))
  }

  test("compose multiple pathPrefix to match on multiple sequential path segments") {
    val postsHandler: Request => String =
      const("Posts Handler!") _ &
        Lens.outside(pathPrefix("index")).set(const("Post Index"))

    val server =
      serveRequest &
        Lens.outside(pathPrefix("users")).set(userHandler) &
        Lens.outside(pathPrefix("posts")).set(postsHandler)

    assertResult("Posts Handler!")(server(POST(List("posts"), "My new post")))
    assertResult("Posts Handler!")(server(GET(List("posts"))))
    assertResult("Post Index")(server(GET(List("posts", "index"))))
  }

  test("react to different HTTP Verbs") {
    val postsHandler: Request => String =
      const("404 Not Found") _ &
        Lens.outside(post).set("Created post with body: " |+| _._2) &
        Lens.outside(get).set(path => "Fetching post at path: " |+| path.intercalate("/")) &
        Lens.outside(delete).set(path => "Deleting post at path: " |+| path.intercalate("/"))

    val server =
      serveRequest &
        Lens.outside(pathPrefix("users")).set(userHandler) &
        Lens.outside(pathPrefix("posts")).set(postsHandler)

    assertResult("Fetching post at path: 12345")(server(GET(List("posts", "12345"))))
    assertResult("Created post with body: My new post")(server(POST(List("posts"), "My new post")))
    assertResult("Deleting post at path: 12345")(server(DELETE(List("posts", "12345"))))
  }
}
