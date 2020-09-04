package proptics.specs

import cats.Eq
import cats.instances.option._
import cats.kernel.laws.discipline.{EqTests, OrderTests}
import cats.laws.discipline.{ExhaustiveCheck, FoldableTests, FunctorTests, MiniInt, ProfunctorTests, TraverseTests}
import proptics.internal.Tagged
import org.scalacheck.ScalacheckShapeless._
import proptics.law.{ChoiceTests, ClosedTests}

class TaggedSpec extends PropticsSuite {
  implicit def eq0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Tagged[Int => Int, Int => Int]] =
    Eq.instance[Tagged[Int => Int, Int => Int]] { (tagged1, tagged2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        tagged1.runTag(int) === tagged2.runTag(int)
      }
    }

  implicit def eq1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Tagged[Int => Int => Int, Int => Int => Int]] =
    Eq.instance[Tagged[Int => Int => Int, Int => Int => Int]] { (tagged1, tagged2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        tagged1.runTag(int)(int) === tagged2.runTag(int)(int)
      }
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
