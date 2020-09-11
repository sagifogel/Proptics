package proptics.specs
import proptics.instances.at._
import proptics.law.discipline.AtTests

class AtSpec extends PropticsSuite {
  checkAll("At Map[Int, Int]", AtTests[Map[Int, Int], Int, Int].at)
}
