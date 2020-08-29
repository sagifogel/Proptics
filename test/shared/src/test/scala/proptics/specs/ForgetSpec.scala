package proptics.specs

import cats.Eq
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import cats.laws.discipline.{ExhaustiveCheck, MiniInt, ProfunctorTests, StrongTests}
import proptics.internal.Forget
import org.scalacheck.ScalacheckShapeless._

class ForgetSpec extends PropticsSuite {
  implicit def eqForget0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, Int, Int]] = Eq.instance[Forget[Int, Int, Int]] { (forget1, forget2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      forget1.runForget(int) === forget2.runForget(int)
    }
  }

  implicit def eqForget1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, (Int, Int), Int]] = Eq.instance[Forget[Int, (Int, Int), Int]] { (forget1, forget2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      forget1.runForget((int, int)) === forget2.runForget((int, int))
    }
  }

  implicit def eqForget2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, (Int, Int), (Int, Int)]] = Eq.instance[Forget[Int, (Int, Int), (Int, Int)]] {
    (forget1, forget2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        forget1.runForget((int, int)) === forget2.runForget((int, int))
      }
  }

  implicit def eqForget3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, (Int, (Int, Int)), (Int, (Int, Int))]] =
    Eq.instance[Forget[Int, (Int, (Int, Int)), (Int, (Int, Int))]] { (forget1, forget2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        forget1.runForget((int, (int, int))) === forget2.runForget((int, (int, int)))
      }
    }

  implicit def eqForget4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, ((Int, Int), Int), ((Int, Int), Int)]] =
    Eq.instance[Forget[Int, ((Int, Int), Int), ((Int, Int), Int)]] { (forget1, forget2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        forget1.runForget(((int, int), int)) === forget2.runForget(((int, int), int))
      }
    }

  checkAll("Profunctor Forget", ProfunctorTests[Forget[Int, *, *]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Strong Forget", StrongTests[Forget[Int, *, *]](Forget.strongForget[Int]).strong[Int, Int, Int, Int, Int, Int])
}
