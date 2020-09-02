package proptics.specs

import cats.instances.map._
import cats.instances.int._
import proptics.law.IndexTests
import proptics.instances.index._

class IndexSpec extends PropticsSuite {
  checkAll("Index Map[Int, Int]", IndexTests[Map[Int, Int], Int, Int].index)
}
