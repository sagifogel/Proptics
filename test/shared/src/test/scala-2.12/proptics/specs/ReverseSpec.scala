package proptics.specs

import proptics.instances.reverse._
import proptics.law.discipline.ReverseTests

class ReverseSpec extends ReverseSpec0 {
  checkAll("ReverseTests[Stream[Int], Stream[Int]] reverse", ReverseTests[Stream[Int], Stream[Int]].reverse)
}
