package proptics.specs

import scala.collection.immutable.Stream

import proptics.instances.each._
import proptics.law.discipline.EachTests

class EachSpec extends EachSpec0 {
  checkAll("Each[Stream[Int], Int] Each", EachTests[Stream[Int], Int].each)
}
