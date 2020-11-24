package proptics.specs
import scala.collection.immutable.ArraySeq

import proptics.instances.index._
import proptics.law.discipline.IndexTests

class IndexSpec extends IndexSpec0 {
  checkAll("Index LazyList[Int]", IndexTests[LazyList[Int], Int, Int].index)
  checkAll("Index ArraySeq[Int]", IndexTests[ArraySeq[Int], Int, Int].index)
}
