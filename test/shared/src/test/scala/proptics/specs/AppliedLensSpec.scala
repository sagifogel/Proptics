package proptics.specs

import cats.instances.option.catsStdInstancesForOption
import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.syntax.option._
import cats.{Eq, Id}

import proptics._
import proptics.syntax.all._

class AppliedLensSpec extends PropticsSuite {
  val wholeLens: AppliedLens[Whole, Int] = whole9.lens(_.part)

  implicit def eqArrow(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => Int] = Eq.instance[Int => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int) === f2(int)
    }
  }

  test("view") {
    wholeLens.view shouldEqual 9
  }

  test("set") {
    wholeLens.set(1) shouldEqual Whole(1)
  }

  test("over") {
    wholeLens.over(_ + 1) shouldEqual Whole(10)
  }
  test("traverse") {
    wholeLens.traverse(_.some) shouldEqual Some(Whole(9))
    wholeLens.traverse(_.some) shouldEqual wholeLens.overF(_.some)
  }

  test("find") {
    wholeLens.find(greaterThan5) shouldEqual Some(9)
    wholeLens.find(greaterThan10) shouldEqual None
  }

  test("exists") {
    wholeLens.exists(greaterThan5) shouldEqual true
    wholeLens.exists(greaterThan10) shouldEqual false
  }

  test("notExists") {
    wholeLens.notExists(greaterThan10) shouldEqual true
    wholeLens.notExists(greaterThan5) shouldEqual false
    wholeLens.notExists(greaterThan5) shouldEqual !wholeLens.exists(greaterThan5)
  }

  test("contains") {
    wholeLens.contains(9) shouldEqual true
    wholeLens.contains(5) shouldEqual false
  }

  test("notContains") {
    wholeLens.notContains(5) shouldEqual true
    wholeLens.notContains(9) shouldEqual false
    wholeLens.notContains(9) shouldEqual !wholeLens.contains(9)
  }

  test("use") {
    wholeLens.use.runA(whole9).value shouldEqual 9
  }

  test("zipWith") {
    wholeLens.zipWith(whole9)(_ + _) shouldEqual Whole(18)
  }

  test("cotraverse") {
    wholeLens.cotraverse[Id](identity) shouldEqual whole9
  }

  test("zipWithF") {
    wholeLens.zipWithF[Id](identity) shouldEqual whole9
  }
}
