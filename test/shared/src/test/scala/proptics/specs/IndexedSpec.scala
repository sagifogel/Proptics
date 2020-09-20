package proptics.specs

import cats.Eq
import cats.arrow.{Profunctor, Strong}
import cats.laws.discipline.{ExhaustiveCheck, MiniInt, ProfunctorTests, StrongTests}
import cats.syntax.either._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import proptics.internal.Indexed
import proptics.law.discipline._
import proptics.profunctor.Wander._
import org.scalacheck.ScalacheckShapeless._
import proptics.profunctor.{Choice, Wander}

class IndexedSpec extends PropticsSuite {
  implicit val wanderIndexed: Wander[Indexed[* => *, Int, *, *]] = Indexed.wanderIndexed[* => *, Int](wanderFunction)
  implicit val choiceIndexed: Choice[Indexed[* => *, Int, *, *]] = Indexed.choiceIndexed[* => *, Int](Choice.choiceFunction)
  implicit val strongIndexed: Strong[Indexed[* => *, Int, *, *]] = Indexed.strongIndexed[* => *, Int](Profunctor.catsStrongForFunction1)
  implicit val profunctorIndexed: Profunctor[Indexed[* => *, Int, *, *]] = Indexed.profunctorIndexed[* => *, Int](Profunctor.catsStrongForFunction1)
  implicit def eqIndexed0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, Int, Int]] = Eq.instance[Indexed[* => *, Int, Int, Int]] {
    (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, int)) === indexed2.runIndex((int, int))
      }
  }

  implicit def eqIndexed1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, (Int, Int), Int]] = Eq.instance[Indexed[* => *, Int, (Int, Int), Int]] {
    (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, (int, int))) === indexed2.runIndex((int, (int, int)))
      }
  }

  implicit def eqIndexed2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, (Int, Int), (Int, Int)]] =
    Eq.instance[Indexed[* => *, Int, (Int, Int), (Int, Int)]] { (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, (int, int))) === indexed2.runIndex((int, (int, int)))
      }
    }

  implicit def eqIndexed3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, (Int, (Int, Int)), (Int, (Int, Int))]] =
    Eq.instance[Indexed[* => *, Int, (Int, (Int, Int)), (Int, (Int, Int))]] { (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, (int, (int, int)))) === indexed2.runIndex((int, (int, (int, int))))
      }
    }

  implicit def eqIndexed4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, ((Int, Int), Int), ((Int, Int), Int)]] =
    Eq.instance[Indexed[* => *, Int, ((Int, Int), Int), ((Int, Int), Int)]] { (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, ((int, int), int))) === indexed2.runIndex((int, ((int, int), int)))
      }
    }

  implicit def eqIndexed5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, Int, Either[Int, Int]]] =
    Eq.instance[Indexed[* => *, Int, Int, Either[Int, Int]]] { (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, int)) === indexed2.runIndex((int, int))
      }
    }

  implicit def eqIndexed6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, Either[Int, Int], Either[Int, Int]]] =
    Eq.instance[Indexed[* => *, Int, Either[Int, Int], Either[Int, Int]]] { (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, int.asRight[Int])) === indexed2.runIndex((int, int.asRight[Int]))
      }
    }

  implicit def eqIndexed7(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Eq.instance[Indexed[* => *, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] { (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, int.asRight[Either[Int, Int]])) === indexed2.runIndex((int, int.asRight[Either[Int, Int]]))
      }
    }

  implicit def eqIndexed8(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Indexed[* => *, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Eq.instance[Indexed[* => *, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] { (indexed1, indexed2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        indexed1.runIndex((int, int.asLeft[Either[Int, Int]])) === indexed2.runIndex((int, int.asLeft[Either[Int, Int]]))
      }
    }

  checkAll("Profunctor Indexed[* => *, Int, Int, Int]", ProfunctorTests[Indexed[* => *, Int, *, *]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Profunctor Indexed[* => *, Int, Int, Int]", StrongTests[Indexed[* => *, Int, *, *]].strong[Int, Int, Int, Int, Int, Int])
  checkAll("Choice Indexed[* => *, Int, Int, Int]", ChoiceTests[Indexed[* => *, Int, *, *]].choice[Int, Int, Int, Int, Int, Int])
  checkAll("Wander Indexed[* => *, Int, Int, Int]", WanderTests[Indexed[* => *, Int, *, *]].wander[Int, Int, Int, Int, Int, Int])
}
