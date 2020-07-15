package proptics.specs

import cats.instances.int._
import cats.instances.tuple._
import cats.syntax.bifunctor._
import cats.instances.option._
import cats.syntax.option._
import org.scalacheck.Arbitrary._
import org.typelevel.discipline.Laws
import proptics.{ALens, Lens}
import proptics.internal.Shop
import proptics.law.{ALensRules, LensRules}
import proptics.instances.tuple._

import scala.Function.const

class ALensSpec extends PropticsSuite {
  val aLens: ALens[Whole, Int] = ALens[Whole, Int](_.focus)(w => i => w.copy(focus = i))
  val ruleSetIdentityLens: Laws#RuleSet = ALensRules(ALens[Int, Int](identity)(const(identity)))
  def ruleSetApply(aLens: ALens[Whole, Int]): Laws#RuleSet = ALensRules(aLens)

  checkAll("ALens apply", ruleSetApply(aLens))
  checkAll("ALens identity", ruleSetIdentityLens)
  checkAll("ALens asLens", LensRules(aLens.asLens))
  checkAll("ALens id", ALensRules(ALens.id[Int]))

  test("view") {
    aLens.view(whole9) shouldEqual 9
  }

  test("set") {
    aLens.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    aLens.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
  test("traverse") {
    aLens.traverse(whole9)(_.some) shouldEqual Some(whole9)
    aLens.traverse(whole9)(_.some) shouldEqual aLens.overF(_.some)(whole9)
  }

  test("find") {
    aLens.find(greaterThan5)(whole9) shouldEqual Some(9)
    aLens.find(greaterThan10)(whole9) shouldEqual None
  }

  test("exists") {
    aLens.exists(greaterThan5)(whole9) shouldEqual true
    aLens.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    aLens.notExists(greaterThan10)(whole9) shouldEqual true
    aLens.notExists(greaterThan5)(whole9) shouldEqual false
    aLens.notExists(greaterThan5)(whole9) shouldEqual (!aLens.exists(greaterThan5)(whole9))
  }

  test("contains") {
    aLens.contains(whole9)(9) shouldEqual true
    aLens.contains(whole9)(5) shouldEqual false
  }

  test("notContains") {
    aLens.notContains(whole9)(5) shouldEqual true
    aLens.notContains(whole9)(9) shouldEqual false
    aLens.notContains(whole9)(9) shouldEqual (!aLens.contains(whole9)(9))
  }

  test("use") {
    aLens.use.runA(whole9).value shouldEqual 9
  }

  test("withLens") {
    val shop = aLens.withLens[Shop[Int, Int, Whole, Whole]](get => set => Shop(get, set))

    shop.set(whole9)(0) shouldEqual Whole(0)
  }

  test("lensStore") {
    sealed trait ADT
    case class IntWrapper(value: Int) extends ADT
    case class TupleWrapper(value: (Boolean, Int)) extends ADT

    val adtLens: Lens[ADT, Int] = Lens.lens {
      case IntWrapper(value) =>
        ALens.id[Int].lensStore(value).bimap(identity, fn => i => IntWrapper(fn(i)))
      case TupleWrapper(value) =>
        _2A[Int, Int, Boolean].lensStore(value).bimap(identity, fn => i => TupleWrapper(fn(i)))
    }

    adtLens.view(IntWrapper(9)) shouldEqual 9
    adtLens.view(TupleWrapper((true, 9))) shouldEqual 9
    adtLens.set(9)(IntWrapper(0)) shouldEqual IntWrapper(9)
    adtLens.set(9)(TupleWrapper((true, 0))) shouldEqual TupleWrapper((true, 9))
  }
}
