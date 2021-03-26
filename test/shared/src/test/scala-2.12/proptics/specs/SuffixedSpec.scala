package proptics.specs

import proptics.instances.suffixed._
import proptics.law.discipline.SuffixedTests

class SuffixedSpec extends SuffixedSpec0 {
  checkAll("SuffixedTests[Stream[Int], Stream[Int]] suffixed", SuffixedTests[Stream[Int], Stream[Int]].suffixed)
}
