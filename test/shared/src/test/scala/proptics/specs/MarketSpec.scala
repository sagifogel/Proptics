package proptics.specs

import cats.Eq
import cats.syntax.either._
import cats.laws.discipline.{ExhaustiveCheck, FunctorTests, MiniInt, ProfunctorTests}
import proptics.internal.Market._
import proptics.internal.Market
import org.scalacheck.ScalacheckShapeless._
import proptics.law.ChoiceTests

class MarketSpec extends PropticsSuite {
  implicit def eqMarket0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, Int, Int]] = Eq.instance[Market[Int, Int, Int, Int]] { (market1, market2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      market1.viewOrModify(int) === market2.viewOrModify(int) && market1.review(int) === market2.review(int)
    }
  }
  implicit def eqMarket1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, (Int, Int), Int]] = Eq.instance[Market[Int, Int, (Int, Int), Int]] {
    (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify((int, int)) === market2.viewOrModify((int, int)) && market1.review(int) === market2.review(int)
      }
  }

  implicit def eqMarket2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, (Int, Int), (Int, Int)]] =
    Eq.instance[Market[Int, Int, (Int, Int), (Int, Int)]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify((int, int)) === market2.viewOrModify((int, int)) && market1.review(int) === market2.review(int)
      }
    }

  implicit def eqMarket3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, ((Int, Int), Int), (Int, Int)]] =
    Eq.instance[Market[Int, Int, ((Int, Int), Int), (Int, Int)]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify(((int, int), int)) === market2.viewOrModify(((int, int), int)) && market1.review(int) === market2.review(int)
      }
    }

  implicit def eqMarket4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, (Int, (Int, Int)), (Int, (Int, Int))]] =
    Eq.instance[Market[Int, Int, (Int, (Int, Int)), (Int, (Int, Int))]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify((int, (int, int))) === market2.viewOrModify((int, (int, int))) && market1.review(int) === market2.review(int)

      }
    }

  implicit def eqMarket5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, ((Int, Int), Int), ((Int, Int), Int)]] =
    Eq.instance[Market[Int, Int, ((Int, Int), Int), ((Int, Int), Int)]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify(((int, int), int)) === market2.viewOrModify(((int, int), int)) && market1.review(int) === market2.review(int)

      }
    }

  implicit def eqMarket6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, Int, Either[Int, Int]]] =
    Eq.instance[Market[Int, Int, Int, Either[Int, Int]]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify(int) === market2.viewOrModify(int) && market1.review(int) === market2.review(int)

      }
    }

  implicit def eqMarket7(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, Either[Int, Int], Either[Int, Int]]] =
    Eq.instance[Market[Int, Int, Either[Int, Int], Either[Int, Int]]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify(int.asRight[Int]) === market2.viewOrModify(int.asRight[Int]) && market1.review(int) === market2.review(int)

      }
    }

  implicit def eqMarket8(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, Either[Either[Int, Int], Int], Int]] =
    Eq.instance[Market[Int, Int, Either[Either[Int, Int], Int], Int]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify(int.asRight[Either[Int, Int]]) === market2.viewOrModify(int.asRight[Either[Int, Int]]) && market1.review(int) === market2.review(int)
      }
    }

  implicit def eqMarket9(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Eq.instance[Market[Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify(int.asRight[Either[Int, Int]]) === market2.viewOrModify(int.asRight[Either[Int, Int]]) && market1.review(int) === market2.review(int)
      }
    }

  implicit def eqMarket10(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Market[Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Eq.instance[Market[Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] { (market1, market2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        market1.viewOrModify(int.asLeft[Either[Int, Int]]) === market2.viewOrModify(int.asLeft[Either[Int, Int]]) && market1.review(int) === market2.review(int)
      }
    }

  checkAll("Functor Market[Int, Int, Int, Int]", FunctorTests[Market[Int, Int, Int, *]].functor[Int, Int, Int])
  checkAll("Profunctor Market[Int, Int, Int, Int]", ProfunctorTests[Market[Int, Int, *, *]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Choice Market[Int, Int, Int, Int]", ChoiceTests[Market[Int, Int, *, *]].choice[Int, Int, Int, Int, Int, Int])
}
