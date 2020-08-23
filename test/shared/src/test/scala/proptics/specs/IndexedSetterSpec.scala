package proptics.specs

import cats.instances.int._
import proptics.specs.Whole._
import proptics.IndexedSetter
import proptics.law.{IndexedSetterRules, SetterRules}

class IndexedSetterSpec extends PropticsSuite {
  val indexedSetter: IndexedSetter[Int, Whole, Int] = IndexedSetter[Int, Whole, Int](fromPair => w => w.copy(part = fromPair(0, w.part)))

  checkAll("IndexedSetter apply", IndexedSetterRules(indexedSetter))
  checkAll("IndexedSetter asSetter", SetterRules(indexedSetter.asSetter))
  
  test("set") {
    indexedSetter.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    indexedSetter.over(_._2 + 1)(Whole(8)) shouldEqual whole9
  }
}
