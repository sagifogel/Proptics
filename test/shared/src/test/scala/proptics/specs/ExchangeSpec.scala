package proptics.specs

import cats.Eq
import cats.laws.discipline.{ExhaustiveCheck, FunctorTests, MiniInt, ProfunctorTests}
import proptics.internal.Exchange
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck.ScalacheckShapeless._

class ExchangeSpec extends PropticsSuite {
  implicit def eqExchange0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Exchange[Int, Int, Int, Int]] = Eq.instance[Exchange[Int, Int, Int, Int]] { (ex1, ex2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      ex1.view(int) === ex2.view(int) && ex1.review(int) === ex2.review(int)
    }
  }

  checkAll("Functor Exchange[Int, Int, Int, Int]", FunctorTests[Exchange[Int, Int, Int, *]].functor[Int, Int, Int])
  checkAll("Profunctor Exchange[Int, Int, Int, Int]", ProfunctorTests[Exchange[Int, Int, *, *]].profunctor[Int, Int, Int, Int, Int, Int])
}
