package proptics.specs

import proptics.law.discipline.EachTests
import proptics.instances.each._

class EachSpec extends EachSpec0{
  checkAll("Each[Stream[Int], Int] Each", EachTests[Stream[Int], Int].each)
}
