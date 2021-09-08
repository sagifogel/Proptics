package proptics.specs

import scala.Function.const

import cats.syntax.option._
import spire.algebra.Field
import spire.std.boolean._

import proptics.syntax.setter._
import proptics.{Setter, Setter_}

trait SetterCompatSuite extends PropticsSuite {
  implicit val intField: Field[Int] with Field.WithDefaultGCD[Int] = new Field[Int] with Field.WithDefaultGCD[Int] {
    override def negate(x: Int): Int = Math.negateExact(x)

    override def zero: Int = 0

    override def one: Int = 1

    override def times(x: Int, y: Int): Int = x * y

    override def plus(x: Int, y: Int): Int = x + y

    override def div(x: Int, y: Int): Int = x / y
  }

  val wholeSetter: Setter[Whole, Int]
  val fromFunctor: Setter[List[Int], Int]
  val boolSetter: Setter[Boolean, Boolean]
  val setterOption: Setter_[List[Int], List[Option[Int]], Int, Option[Int]]

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
