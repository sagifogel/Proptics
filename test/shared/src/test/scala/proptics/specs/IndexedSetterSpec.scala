package proptics.specs
import proptics.law.discipline._
import proptics.specs.Whole._
import proptics.specs.compose._
import proptics.{IndexedSetter, Iso}

class IndexedSetterSpec extends PropticsSuite {
  val wholeIndexedSetter: IndexedSetter[Int, Whole, Int] = IndexedSetter[Int, Whole, Int](fromPair => w => w.copy(part = fromPair(w.part, 0)))

  checkAll("IndexedSetter[Int, Whole, Int] apply", IndexedSetterTests(wholeIndexedSetter).indexedSetter)
  checkAll("IndexedSetter[Int, Whole, Int] asSetter", SetterTests(wholeIndexedSetter.asSetter).setter)
  checkAll("Iso[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedSetterTests(Iso.id[Int] compose indexedSetter).indexedSetter)
  checkAll("IndexedSetter[Int, Int, Int] <<* IndexedLens[Int, Int, Int]", IndexedSetterTests(indexedSetter <<* indexedLens).indexedSetter)
  checkAll("IndexedSetter[Int, Int, Int] *>> IndexedLens[Int, Int, Int]", IndexedSetterTests(indexedSetter *>> indexedLens).indexedSetter)
  checkAll("IndexedSetter[Int, Int, Int] <<* AnIndexedLens[Int, Int, Int]", IndexedSetterTests(indexedSetter <<* anIndexedLens).indexedSetter)
  checkAll("IndexedSetter[Int, Int, Int] *>> AnIndexedLens[Int, Int, Int]", IndexedSetterTests(indexedSetter *>> anIndexedLens).indexedSetter)
  checkAll("IndexedSetter[Int, Int, Int] <<* IndexedTraversal[Int, Int, Int]", IndexedSetterTests(indexedSetter <<* indexedTraversal).indexedSetter)
  checkAll("IndexedSetter[Int, Int, Int] *>> IndexedTraversal[Int, Int, Int]", IndexedSetterTests(indexedSetter *>> indexedTraversal).indexedSetter)
  checkAll("IndexedSetter[Int, Int, Int] <<* IndexedSetter[Int, Int, Int]", IndexedSetterTests(indexedSetter <<* indexedSetter).indexedSetter)
  checkAll("IndexedSetter[Int, Int, Int] *>> IndexedSetter[Int, Int, Int]", IndexedSetterTests(indexedSetter *>> indexedSetter).indexedSetter)

  test("set") {
    wholeIndexedSetter.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    wholeIndexedSetter.over(_._1 + 1)(Whole(8)) shouldEqual whole9
  }

  test("reindex") {
    wholeIndexedSetter.reindex(_.toString).set(9)(Whole(9)) shouldEqual whole9
  }
}
