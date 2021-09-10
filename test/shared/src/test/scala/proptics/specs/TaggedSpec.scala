package proptics.specs

import cats.Eq
import cats.Eq._
import cats.kernel.laws.discipline.{EqTests, OrderTests}
import cats.laws.discipline.{ExhaustiveCheck, FoldableTests, FunctorTests, MiniInt, ProfunctorTests, TraverseTests}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Gen}

import proptics.internal.Tagged
import proptics.law.discipline.{ChoiceTests, ClosedTests}

class TaggedSpec extends PropticsSuite {
  implicit def eq0: Eq[Tagged[String, Int]] =
    Eq.instance[Tagged[String, Int]]((tagged1, tagged2) => tagged1.runTag === tagged2.runTag)

  implicit def eq1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Tagged[Int => Int, Int => Int]] =
    Eq.instance[Tagged[Int => Int, Int => Int]] { (tagged1, tagged2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        tagged1.runTag(int) === tagged2.runTag(int)
      }
    }

  implicit def eq2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Tagged[Int => Int => Int, Int => Int => Int]] =
    Eq.instance[Tagged[Int => Int => Int, Int => Int => Int]] { (tagged1, tagged2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        tagged1.runTag(int)(int) === tagged2.runTag(int)(int)
      }
    }

  implicit def cogenTagged: Cogen[Tagged[String, Int]] = Cogen.cogenInt.contramap(_.runTag)

  implicit def arbTagged0: Arbitrary[Tagged[String, Int]] = Arbitrary[Tagged[String, Int]] {
    for {
      i <- Arbitrary.arbInt.arbitrary
    } yield Tagged[String, Int](i)
  }

  implicit def arbTagged1: Arbitrary[Tagged[Int, Int]] = Arbitrary[Tagged[Int, Int]] {
    for {
      i <- Arbitrary.arbInt.arbitrary
    } yield Tagged[Int, Int](i)
  }

  implicit def arbTagged2: Arbitrary[Tagged[String, Int] => Tagged[String, Int]] = Arbitrary[Tagged[String, Int] => Tagged[String, Int]] {
    for {
      tagged2Tagged <- Gen.function1[Tagged[String, Int], Tagged[String, Int]](arbTagged0.arbitrary)
    } yield tagged2Tagged
  }

  implicit def arbTagged3: Arbitrary[Tagged[String, Option[Int]]] = Arbitrary[Tagged[String, Option[Int]]] {
    for {
      runTag <- Arbitrary.arbOption[Int](Arbitrary.arbInt).arbitrary
    } yield Tagged[String, Option[Int]](runTag)
  }

  checkAll("Eq Tagged[String, Int]", EqTests[Tagged[String, Int]].eqv)
  checkAll("Order Tagged[String, Int]", OrderTests[Tagged[String, Int]].order)
  checkAll("Functor Tagged[String, Int]", FunctorTests[Tagged[String, *]].functor[Int, Int, Int])
  checkAll("Profunctor Tagged[Int, Int]", ProfunctorTests[Tagged](Tagged.profunctorTagged).profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Choice Tagged[Int, Int]", ChoiceTests[Tagged].choice[Int, Int, Int, Int, Int, Int])
  checkAll("Closed Tagged[Int, Int]", ClosedTests[Tagged].closed[Int, Int, Int, Int, Int, Int])
  checkAll("Foldable Tagged[String, Int]", FoldableTests[Tagged[String, *]].foldable[Int, Int])
  checkAll("Traverse Tagged[String, Int]", TraverseTests[Tagged[String, *]].traverse[Int, Int, Int, Int, Option, Option])
}
