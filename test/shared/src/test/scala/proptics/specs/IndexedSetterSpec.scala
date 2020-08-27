package proptics.specs

import cats.instances.int._
import proptics.specs.Whole._
import proptics.IndexedSetter
import proptics.law.{IndexedSetterRules, SetterRules}
import proptics.specs.Compose._

class IndexedSetterSpec extends PropticsSuite {
  val wholeIndexedSetter: IndexedSetter[Int, Whole, Int] = IndexedSetter[Int, Whole, Int](fromPair => w => w.copy(part = fromPair(0, w.part)))

  checkAll("IndexedSetter apply", IndexedSetterRules(wholeIndexedSetter))
  checkAll("IndexedSetter asSetter", SetterRules(wholeIndexedSetter.asSetter))
  checkAll("compose with IndexedLens", IndexedSetterRules(indexedSetter compose indexedLens))
  checkAll("compose with AnIndexedLens", IndexedSetterRules(indexedSetter compose anIndexedLens))
  checkAll("compose with IndexedTraversal", IndexedSetterRules(indexedSetter compose indexedTraversal))
  checkAll("compose with IndexedSetter", IndexedSetterRules(indexedSetter compose indexedSetter))

  test("set") {
    wholeIndexedSetter.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    wholeIndexedSetter.over(_._2 + 1)(Whole(8)) shouldEqual whole9
  }
}
