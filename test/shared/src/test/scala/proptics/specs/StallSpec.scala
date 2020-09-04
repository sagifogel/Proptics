package proptics.specs

import cats.Eq
import cats.syntax.either._
import cats.arrow.Profunctor
import cats.laws.discipline.{ExhaustiveCheck, FunctorTests, MiniInt, ProfunctorTests, StrongTests}
import proptics.internal.Stall._
import proptics.internal.Stall
import org.scalacheck.ScalacheckShapeless._
import proptics.law.ChoiceTests

class StallSpec extends PropticsSuite {
  implicit val profunctorStall: Profunctor[Stall[Int, Int, *, *]] = Stall.profunctorStall[Int, Int]

  implicit def eqStall0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, Int, Int]] = Eq.instance[Stall[Int, Int, Int, Int]] { (stall1, stall2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      stall1.viewOrModify(int) === stall2.viewOrModify(int) && stall1.set(int)(int) === stall2.set(int)(int)
    }
  }

  implicit def eqStall1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, (Int, Int), Int]] = Eq.instance[Stall[Int, Int, (Int, Int), Int]] { (stall1, stall2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      stall1.viewOrModify((int, int)) === stall2.viewOrModify((int, int)) &&
      stall1.set((int, int))(int) === stall2.set((int, int))(int)
    }
  }

  implicit def eqStall2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, (Int, Int), (Int, Int)]] =
    Eq.instance[Stall[Int, Int, (Int, Int), (Int, Int)]] { (stall1, stall2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        stall1.viewOrModify((int, int)) === stall2.viewOrModify((int, int)) &&
        stall1.set((int, int))(int) === stall2.set((int, int))(int)
      }
    }

  implicit def eqStall3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, (Int, (Int, Int)), (Int, (Int, Int))]] =
    Eq.instance[Stall[Int, Int, (Int, (Int, Int)), (Int, (Int, Int))]] { (stall1, stall2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        stall1.viewOrModify((int, (int, int))) === stall2.viewOrModify((int, (int, int))) &&
        stall1.set((int, (int, int)))(int) === stall2.set((int, (int, int)))(int)
      }
    }

  implicit def eqStall4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, ((Int, Int), Int), ((Int, Int), Int)]] =
    Eq.instance[Stall[Int, Int, ((Int, Int), Int), ((Int, Int), Int)]] { (stall1, stall2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        stall1.viewOrModify(((int, int), int)) === stall2.viewOrModify(((int, int), int)) &&
        stall1.set(((int, int), int))(int) === stall2.set(((int, int), int))(int)

      }
    }

  implicit def eqStall5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, Int, Either[Int, Int]]] =
    Eq.instance[Stall[Int, Int, Int, Either[Int, Int]]] { (stall1, stall2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        stall1.viewOrModify(int) === stall2.viewOrModify(int) && stall1.set(int)(int) === stall2.set(int)(int)

      }
    }

  implicit def eqStall6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, Either[Int, Int], Either[Int, Int]]] =
    Eq.instance[Stall[Int, Int, Either[Int, Int], Either[Int, Int]]] { (stall1, stall2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Int]

        stall1.viewOrModify(either) === stall2.viewOrModify(either) && stall1.set(either)(int) === stall2.set(either)(int)
      }
    }

  implicit def eqStall7(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Eq.instance[Stall[Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] { (stall1, stall2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Either[Int, Int]]

        stall1.viewOrModify(either) === stall2.viewOrModify(either) && stall1.set(either)(int) === stall2.set(either)(int)
      }
    }

  implicit def eqStall8(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Stall[Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Eq.instance[Stall[Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] { (stall1, stall2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asLeft[Either[Int, Int]]

        stall1.viewOrModify(either) === stall2.viewOrModify(either) && stall1.set(either)(int) === stall2.set(either)(int)
      }
    }

  checkAll("Functor Stall[Int, Int, Int, Int]", FunctorTests[Stall[Int, Int, Int, *]].functor[Int, Int, Int])
  checkAll("Profunctor Stall[Int, Int, Int, Int]", ProfunctorTests[Stall[Int, Int, *, *]](profunctorStall).profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Strong Stall[Int, Int, Int, Int]", StrongTests[Stall[Int, Int, *, *]].strong[Int, Int, Int, Int, Int, Int])
  checkAll("Choice Stall[Int, Int, Int, Int]", ChoiceTests[Stall[Int, Int, *, *]].choice[Int, Int, Int, Int, Int, Int])
}
