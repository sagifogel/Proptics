package proptics.specs

import cats.instances.map._
import cats.instances.int._
import proptics.law.IndexRules
import proptics.instances.index._

class IndexSpec extends PropticsSuite {
  checkAll("Index Map[Int, Int]", IndexRules[Map[Int, Int], Int, Int])
}
