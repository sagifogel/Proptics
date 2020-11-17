package proptics.specs

import scala.Function.const

import cats.Eq
import cats.laws.discipline.{ExhaustiveCheck, FunctorTests, MiniInt, ProfunctorTests, StrongTests}
import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Cogen.cogenInt
import org.scalacheck.ScalacheckShapeless._

import proptics.internal.Zipping
import proptics.internal.Zipping._
import proptics.law.discipline.ClosedTests

class ZippingSpec extends PropticsSuite {
  implicit def eqZipping0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Zipping[Int, Int]] = Eq.instance[Zipping[Int, Int]] { (zipping1, zipping2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      zipping1.runZipping(int)(int) === zipping2.runZipping(int)(int)
    }
  }

  implicit def eqZipping1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Zipping[(Int, Int), Int]] =
    Eq.instance[Zipping[(Int, Int), Int]] { (zipping1, zipping2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        zipping1.runZipping((int, int))((int, int)) === zipping2.runZipping((int, int))((int, int))
      }
    }

  implicit def eqZipping2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Zipping[(Int, Int), (Int, Int)]] =
    Eq.instance[Zipping[(Int, Int), (Int, Int)]] { (zipping1, zipping2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        zipping1.runZipping((int, int))((int, int)) === zipping2.runZipping((int, int))((int, int))
      }
    }

  implicit def eqZipping3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Zipping[((Int, Int), Int), ((Int, Int), Int)]] =
    Eq.instance[Zipping[((Int, Int), Int), ((Int, Int), Int)]] { (zipping1, zipping2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        zipping1.runZipping(((int, int), int))(((int, int), int)) === zipping2.runZipping(((int, int), int))(((int, int), int))
      }
    }

  implicit def eqZipping4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Zipping[(Int, (Int, Int)), (Int, (Int, Int))]] =
    Eq.instance[Zipping[(Int, (Int, Int)), (Int, (Int, Int))]] { (zipping1, zipping2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        zipping1.runZipping((int, (int, int)))((int, (int, int))) === zipping2.runZipping((int, (int, int)))((int, (int, int)))
      }
    }

  implicit def eqZipping5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Zipping[Int => Int, Int => Int]] =
    Eq.instance[Zipping[Int => Int, Int => Int]] { (zipping1, zipping2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        zipping1.runZipping(identity)(identity)(int) === zipping2.runZipping(identity)(identity)(int)
      }
    }

  implicit def eqZipping6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Zipping[Int => Int => Int, Int => Int => Int]] =
    Eq.instance[Zipping[Int => Int => Int, Int => Int => Int]] { (zipping1, zipping2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        zipping1.runZipping(const(identity))(const(identity))(int)(int) === zipping2.runZipping(const(identity))(const(identity))(int)(int)
      }
    }

  checkAll("Functor Zipping[Int, Int]", FunctorTests[Zipping[Int, *]].functor[Int, Int, Int])
  checkAll("Profunctor Zipping[Int, Int]", ProfunctorTests[Zipping](profunctorZipping).profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Strong Zipping[Int, Int]", StrongTests[Zipping].strong[Int, Int, Int, Int, Int, Int])
  checkAll("Closed Zipping[Int, Int]", ClosedTests[Zipping].closed[Int, Int, Int, Int, Int, Int])
}
