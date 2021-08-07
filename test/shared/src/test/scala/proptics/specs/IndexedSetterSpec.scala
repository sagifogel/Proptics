package proptics.specs
import proptics.law.discipline._
import proptics.specs.Whole._
import proptics.specs.compose._
import proptics.{ALens, APrism, AffineTraversal, AnAffineTraversal, AnIso, Grate, IndexedSetter, Iso, Lens, Prism, Setter, Traversal}

class IndexedSetterSpec extends PropticsSuite {
  val wholeIndexedSetter: IndexedSetter[Int, Whole, Int] = IndexedSetter[Int, Whole, Int](fromPair => w => w.copy(part = fromPair(w.part, 0)))

  checkAll("IndexedSetter[Int, Whole, Int] apply", IndexedSetterTests(wholeIndexedSetter).indexedSetter)
  checkAll("IndexedSetter[Int, Whole, Int] asSetter", SetterTests(wholeIndexedSetter.asSetter).setter)
  checkAll("IndexedLens[Int, Int] compose with Iso[Int, Int]", IndexedSetterTests(indexedSetter compose Iso.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with Iso[Int, Int]", IndexedSetterTests(indexedSetter andThen Iso.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with AnIso[Int, Int]", IndexedSetterTests(indexedSetter compose AnIso.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with AnIso[Int, Int]", IndexedSetterTests(indexedSetter andThen AnIso.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with Lens[Int, Int]", IndexedSetterTests(indexedSetter compose Lens.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with Lens[Int, Int]", IndexedSetterTests(indexedSetter andThen Lens.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with ALens[Int, Int]", IndexedSetterTests(indexedSetter compose ALens.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with ALens[Int, Int]", IndexedSetterTests(indexedSetter andThen ALens.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with Prism[Int, Int]", IndexedSetterTests(indexedSetter compose Prism.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with Prism[Int, Int]", IndexedSetterTests(indexedSetter andThen Prism.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with APrism[Int, Int]", IndexedSetterTests(indexedSetter compose APrism.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with APrism[Int, Int]", IndexedSetterTests(indexedSetter andThen APrism.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with AffineTraversal[Int, Int]", IndexedSetterTests(indexedSetter compose AffineTraversal.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with AffineTraversal[Int, Int]", IndexedSetterTests(indexedSetter andThen AffineTraversal.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with AnAffineTraversal[Int, Int]", IndexedSetterTests(indexedSetter compose AnAffineTraversal.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with AnAffineTraversal[Int, Int]", IndexedSetterTests(indexedSetter andThen AnAffineTraversal.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with Traversal[Int, Int]", IndexedSetterTests(indexedSetter compose Traversal.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with Traversal[Int, Int]", IndexedSetterTests(indexedSetter andThen Traversal.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with Setter[Int, Int]", IndexedSetterTests(indexedSetter compose Setter.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with Setter[Int, Int]", IndexedSetterTests(indexedSetter andThen Setter.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] compose with Grate[Int, Int]", IndexedSetterTests(indexedSetter compose Grate.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int] andThen with Grate[Int, Int]", IndexedSetterTests(indexedSetter andThen Grate.id[Int]).indexedSetter)
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
