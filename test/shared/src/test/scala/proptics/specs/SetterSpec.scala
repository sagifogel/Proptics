package proptics.specs

import scala.Function.const

import cats.Show
import cats.syntax.option._
import spire.algebra.Field
import spire.std.boolean._

import proptics.law.discipline._
import proptics.specs.Whole._
import proptics.specs.compose._
import proptics.syntax.setter._
import proptics.{Setter, Setter_}

class SetterSpec extends PropticsSuite {
  implicit val intField: Field[Int] with Field.WithDefaultGCD[Int] = new Field[Int] with Field.WithDefaultGCD[Int] {
    override def negate(x: Int): Int = Math.negateExact(x)

    override def zero: Int = 0

    override def one: Int = 1

    override def times(x: Int, y: Int): Int = x * y

    override def plus(x: Int, y: Int): Int = x + y

    override def div(x: Int, y: Int): Int = x / y
  }

  val boolSetter: Setter[Boolean, Boolean] = Setter[Boolean, Boolean](_.apply)
  val fromFunctor: Setter[List[Int], Int] = Setter.fromFunctor[List, Int]
  val wholeSetter: Setter[Whole, Int] = Setter[Whole, Int](f => w => w.copy(part = f(w.part)))
  val setterOption: Setter_[List[Int], List[Option[Int]], Int, Option[Int]] =
    Setter_[List[Int], List[Option[Int]], Int, Option[Int]](f => _.map(f))
  val fromContravariant: Setter_[Show[Int], Show[List[Int]], List[Int], Int] =
    Setter_.fromContravariant[Show, List[Int], Int]

  checkAll("Setter[Whole, Int] apply", SetterTests(wholeSetter).setter)
  checkAll("Setter[Int, Int] id", SetterTests(Setter.id[Int]).setter)
  checkAll("Setter[List[Int], Int] fromFunctor", SetterTests(fromFunctor).setter)
  checkAll("Setter[Int, Int] compose with Iso[Int, Int]", SetterTests(setter compose iso).setter)
  checkAll("Setter[Int, Int] compose with AnIso[Int, Int]", SetterTests(setter compose anIso).setter)
  checkAll("Setter[Int, Int] compose with Lens[Int, Int]", SetterTests(setter compose lens).setter)
  checkAll("Setter[Int, Int] compose with ALens[Int, Int]", SetterTests(setter compose aLens).setter)
  checkAll("Setter[Int, Int] compose with Prism[Int, Int]", SetterTests(setter compose prism).setter)
  checkAll("Setter[Int, Int] compose with APrism[Int, Int]", SetterTests(setter compose aPrism).setter)
  checkAll("Setter[Int, Int] compose with AffineTraversal[Int, Int]", SetterTests(setter compose affineTraversal).setter)
  checkAll("Setter[Int, Int] compose with AnAffineTraversal[Int, Int]", SetterTests(setter compose anAffineTraversal).setter)
  checkAll("Setter[Int, Int] compose with Traversal[Int, Int]", SetterTests(setter compose traversal).setter)
  checkAll("Setter[Int, Int] compose with ATraversal[Int, Int]", SetterTests(setter compose aTraversal).setter)
  checkAll("Setter[Int, Int] compose with Setter[Int, Int]", SetterTests(setter compose setter).setter)
  checkAll("Setter[Int, Int] compose with Grate[Int, Int]", SetterTests(setter compose grate).setter)
  checkAll("Setter[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedSetterTests(setter compose indexedLens).indexedSetter)
  checkAll("Setter[Int, Int] compose with AnIndexedLens[Int, Int, Int]", IndexedSetterTests(setter compose anIndexedLens).indexedSetter)
  checkAll("Setter[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedSetterTests(setter compose indexedTraversal).indexedSetter)
  checkAll("Setter[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(setter compose indexedSetter).indexedSetter)

  test("set") {
    fromFunctor.set(9)(List(1)) shouldEqual List(9)
    fromContravariant.set(10)(Show.fromToString[Int]).show(list) shouldEqual 10.toString
    wholeSetter.set(9)(Whole(0)) shouldEqual whole9
  }

  test("over") {
    fromFunctor.over(_ + 1)(List.fill(1)(8)) shouldEqual List(9)
    fromContravariant.over(_.length)(Show.fromToString[Int]).show(list) shouldEqual list.length.toString
    wholeSetter.over(_ + 1)(Whole(8)) shouldEqual whole9
  }

  test("addOver") {
    wholeSetter.addOver(Whole(8))(1) shouldEqual whole9
  }

  test("mulOver") {
    wholeSetter.mulOver(Whole(5))(2) shouldEqual Whole(10)
  }

  test("subOver") {
    wholeSetter.subOver(Whole(10))(1) shouldEqual whole9
  }

  test("divOver") {
    wholeSetter.divOver(Whole(8))(2) shouldEqual Whole(4)
  }

  test("disjOver") {
    boolSetter.disjOver(true)(false) shouldEqual true
    boolSetter.disjOver(false)(true) shouldEqual true
    boolSetter.disjOver(false)(false) shouldEqual false
  }

  test("conjOver") {
    boolSetter.conjOver(true)(false) shouldEqual false
    boolSetter.conjOver(false)(true) shouldEqual false
    boolSetter.conjOver(false)(false) shouldEqual false
    boolSetter.conjOver(true)(true) shouldEqual true
  }

  test("appendOver") {
    wholeSetter.appendOver(Whole(8))(2) shouldEqual Whole(10)
  }

  test("setJust") {
    setterOption.setJust(list)(9) shouldEqual list.map(const(9.some))
  }
}
