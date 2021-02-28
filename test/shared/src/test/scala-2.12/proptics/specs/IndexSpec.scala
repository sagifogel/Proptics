package proptics.specs

import proptics.instances.index._
import proptics.law.discipline.IndexTests

class IndexSpec extends IndexSpec0 {
  checkAll("Index[Stream[Int]]", IndexTests[Stream[Int], Int, Int].index)
}
