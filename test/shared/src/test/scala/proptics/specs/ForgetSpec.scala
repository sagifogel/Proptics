package proptics.specs

import cats.Eq
import cats.syntax.either._
import cats.instances.int._
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import cats.laws.discipline.{ExhaustiveCheck, MiniInt, ProfunctorTests, StrongTests}
import proptics.internal.Forget
import org.scalacheck.ScalacheckShapeless._
import proptics.law.{ChoiceTests, CochoiceTests, WanderTests}

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

  implicit def eqForget5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, Int, (Int, Int)]] =
    Eq.instance[Forget[Int, Int, (Int, Int)]] { (forget1, forget2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        forget1.runForget(int) === forget2.runForget(int)
      }
    }

  implicit def eqForget6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, Int, Either[Int, Int]]] =
    Eq.instance[Forget[Int, Int, Either[Int, Int]]] { (forget1, forget2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        forget1.runForget(int) === forget2.runForget(int)
      }
    }

  implicit def eqForget7(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, Either[Int, Int], Either[Int, Int]]] =
    Eq.instance[Forget[Int, Either[Int, Int], Either[Int, Int]]] { (forget1, forget2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Int]

        forget1.runForget(either) === forget2.runForget(either)
      }
    }

  implicit def eqForget8(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Eq.instance[Forget[Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] { (forget1, forget2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Either[Int, Int]]

        forget1.runForget(either) === forget2.runForget(either)
      }
    }

  implicit def eqForget9(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Forget[Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Eq.instance[Forget[Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] { (forget1, forget2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asLeft[Either[Int, Int]]

        forget1.runForget(either) === forget2.runForget(either)
      }
    }

  checkAll("Semigroup Forget[Int, Int, Int]", SemigroupTests[Forget[Int, Int, Int]].semigroup)
  checkAll("Monoid Forget[Int, Int, Int]", MonoidTests[Forget[Int, Int, Int]].monoid)
  checkAll("Profunctor Forget[Int, Int, Int]", ProfunctorTests[Forget[Int, *, *]](Forget.profunctorForget[Int]).profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Strong Forget[Int, Int, Int]", StrongTests[Forget[Int, *, *]](Forget.strongForget[Int](Forget.profunctorForget[Int])).strong[Int, Int, Int, Int, Int, Int])
  checkAll("Choice Forget[Int, Int, Int]", ChoiceTests[Forget[Int, *, *]](Forget.choiceForget[Int]).choice[Int, Int, Int, Int, Int, Int])
  checkAll("Wander Forget[Int, Int, Int]", WanderTests[Forget[Int, *, *]](Forget.wanderForget[Int]).wander[Int, Int, Int, Int, Int, Int])
  checkAll("Cochoice Forget[Int, Int, Int]", CochoiceTests[Forget[Int, *, *]](Forget.cochoiceForget[Int]).cochoice[Int, Int, Int, Int, Int, Int])
}
