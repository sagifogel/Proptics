package proptics.specs

import proptics.instances.cons._
import proptics.law.discipline.ConsTests

class ConsSpec extends ConsSpec0 {
  checkAll("Cons[Stream[Int]]", ConsTests[Stream[Int], Int].cons)
}
