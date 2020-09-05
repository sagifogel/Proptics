package proptics.specs
import proptics.instances.index._
import proptics.law.discipline.IndexTests

class IndexSpec extends PropticsSuite {
  checkAll("Index Map[Int, Int]", IndexTests[Map[Int, Int], Int, Int].index)
}
