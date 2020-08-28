package proptics.specs

import cats.instances.map._
import cats.instances.int._
import proptics.law.AtRules
import proptics.instances.at._

class AtSpec extends PropticsSuite {
  checkAll("At Map[Int, Int]", AtRules[Map[Int, Int], Int, Int])
}
