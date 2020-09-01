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
import proptics.law._
import proptics.instances.tuple._
import proptics.specs.Compose._

import scala.Function.const

class ALensSpec extends PropticsSuite {
  val wholeLens: ALens[Whole, Int] = ALens[Whole, Int](_.part)(w => i => w.copy(part = i))
  val ruleSetIdentityLens: Laws#RuleSet = ALensRules(ALens[Int, Int](identity)(const(identity)))
  def ruleSetApply(aLens: ALens[Whole, Int]): Laws#RuleSet = ALensRules(aLens)

  checkAll("ALens apply", ruleSetApply(wholeLens))
  checkAll("ALens identity", ruleSetIdentityLens)
  checkAll("ALens[Whole, Int] asLens", LensTests(wholeLens.asLens).lens)
  checkAll("ALens id", ALensRules(ALens.id[Int]))
  checkAll("compose with Iso", ALensRules(aLens compose iso))
  checkAll("compose with AnIso", ALensRules(aLens compose anIso))
  checkAll("compose with Lens", ALensRules(aLens compose lens))
  checkAll("compose with ALens", ALensRules(aLens compose aLens))
  checkAll("compose with Prism", TraversalRules(aLens compose prism))
  checkAll("compose with APrism", TraversalRules(aLens compose aPrism))
  checkAll("compose with AffineTraversal", AffineTraversalRules(aLens compose affineTraversal))
  checkAll("compose with AnAffineTraversal", AnAffineTraversalRules(aLens compose anAffineTraversal))
  checkAll("compose with Traversal", TraversalRules(aLens compose traversal))
  checkAll("compose with ATraversal", ATraversalRules(aLens compose aTraversal))
  checkAll("compose with Setter", SetterRules(aLens compose setter))

  test("view") {
    wholeLens.view(whole9) shouldEqual 9
  }

  test("set") {
    wholeLens.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    wholeLens.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
  test("traverse") {
    wholeLens.traverse(whole9)(_.some) shouldEqual Some(whole9)
    wholeLens.traverse(whole9)(_.some) shouldEqual wholeLens.overF(_.some)(whole9)
  }

  test("find") {
    wholeLens.find(greaterThan5)(whole9) shouldEqual Some(9)
    wholeLens.find(greaterThan10)(whole9) shouldEqual None
  }

  test("exists") {
    wholeLens.exists(greaterThan5)(whole9) shouldEqual true
    wholeLens.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    wholeLens.notExists(greaterThan10)(whole9) shouldEqual true
    wholeLens.notExists(greaterThan5)(whole9) shouldEqual false
    wholeLens.notExists(greaterThan5)(whole9) shouldEqual (!wholeLens.exists(greaterThan5)(whole9))
  }

  test("contains") {
    wholeLens.contains(whole9)(9) shouldEqual true
    wholeLens.contains(whole9)(5) shouldEqual false
  }

  test("notContains") {
    wholeLens.notContains(whole9)(5) shouldEqual true
    wholeLens.notContains(whole9)(9) shouldEqual false
    wholeLens.notContains(whole9)(9) shouldEqual (!wholeLens.contains(whole9)(9))
  }

  test("use") {
    wholeLens.use.runA(whole9).value shouldEqual 9
  }

  test("withLens") {
    val shop = wholeLens.withLens[Shop[Int, Int, Whole, Whole]](get => set => Shop(get, set))

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

  test("compose with Getter") {
    (aLens compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (aLens compose fold).fold(9) shouldEqual 9
  }
}
