package proptics.specs

import scala.collection.immutable.{ListMap, SortedMap}

import cats.Eq
import cats.syntax.option._

import proptics.At
import proptics.instances.at._
import proptics.law.discipline.AtTests
import proptics.syntax.at._

class AtSpec extends PropticsSuite {
  implicit val eqListMap: Eq[ListMap[Int, Int]] = Eq.fromUniversalEquals[ListMap[Int, Int]]

  checkAll("At Option[Int]", AtTests[Option[Int], Unit, Int].at)
  checkAll("At Set[Int]", AtTests[Set[Int], Int, Unit].at)
  checkAll("At SortedMap[Int, Int]", AtTests[SortedMap[Int, Int], Int, Int].at)
  checkAll("At ListMap[Int, Int]", AtTests[ListMap[Int, Int], Int, Int].at)
  checkAll("At Map[Int, Int]", AtTests[Map[Int, Int], Int, Int].at)

  test("atSet") {
    val set = Set[String]("A", "B")
    val at = At[Set[String], String, Unit]

    at.at("A").set(None)(set) shouldEqual Set("B")
    at.at("A").set(().some)(set).toList.sorted shouldEqual List("A", "B")
    at.remove("A")(set) shouldEqual Set("B")
  }

  test("atSortedMap") {
    val map = SortedMap[String, String](("A", "A"), ("B", "B"))
    val at = At[SortedMap[String, String], String, String]

    at.at("A").set(None)(map) shouldEqual SortedMap(("B", "B"))
    at.at("A").set("C".some)(map).get("A") shouldEqual "C".some
    at.remove("A")(map).toList shouldEqual List(("B", "B"))
  }

  test("atListMap") {
    val map = ListMap[String, String](("A", "A"), ("B", "B"))
    val at = At[ListMap[String, String], String, String]

    at.at("A").set(None)(map) shouldEqual ListMap(("B", "B"))
    at.at("A").set("C".some)(map).get("A") shouldEqual "C".some
    at.remove("A")(map).toList shouldEqual List(("B", "B"))
  }

  test("atMap") {
    val map = Map[String, String](("A", "A"), ("B", "B"))
    val at = At[Map[String, String], String, String]

    at.at("A").set(None)(map) shouldEqual Map(("B", "B"))
    at.at("A").set("C".some)(map).get("A") shouldEqual "C".some
    at.remove("A")(map).toList shouldEqual List(("B", "B"))
  }
}
