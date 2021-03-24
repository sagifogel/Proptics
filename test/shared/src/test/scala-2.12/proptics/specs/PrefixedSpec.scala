package proptics.specs

import proptics.instances.prefixed._
import proptics.law.discipline.PrefixedTests

class PrefixedSpec extends PrefixedSpec0 {
  checkAll("PrefixedTests[Stream[Int], Stream[Int]] prefixed", PrefixedTests[Stream[Int], Stream[Int]].prefixed)
}
