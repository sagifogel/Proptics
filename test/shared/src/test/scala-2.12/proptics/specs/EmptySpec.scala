package proptics.specs

import proptics.instances.empty._
import proptics.law.discipline.EmptyTests

class EmptySpec extends EmptySpec0 {
  checkAll("Empty[Stream[Int]]", EmptyTests[Stream[Int]].empty)
}
