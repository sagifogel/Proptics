package proptics.specs

import cats.arrow.{Profunctor, Strong}
import cats.laws.discipline.{ExhaustiveCheck, MiniInt, ProfunctorTests, StrongTests}
import cats.syntax.either._
import cats.{Applicative, Eq, Id}
import org.scalacheck.{Arbitrary, Gen}

import proptics.internal.{Bazaar, RunBazaar}
import proptics.law.discipline.{ChoiceTests, WanderTests}
import proptics.profunctor.{Choice, Wander}

class BazaarSpec extends PropticsSuite {
  implicit val profunctorBazaar: Profunctor[Bazaar[* => *, Int, Int, *, *]] = Bazaar.profunctorBazaar[* => *, Int, Int]
  implicit val strongBazaar: Strong[Bazaar[* => *, Int, Int, *, *]] = Bazaar.strongBazaar[* => *, Int, Int]
  implicit val choiceBazaar: Choice[Bazaar[* => *, Int, Int, *, *]] = Bazaar.choiceBazaar[* => *, Int, Int]
  implicit val wanderBazaar: Wander[Bazaar[* => *, Int, Int, *, *]] = Bazaar.wanderBazaar[* => *, Int, Int]

  implicit def arbBazaar: Arbitrary[Bazaar[* => *, Int, Int, Int, Int]] = Arbitrary[Bazaar[* => *, Int, Int, Int, Int]] {
    Gen.const[Bazaar[* => *, Int, Int, Int, Int]](new Bazaar[* => *, Int, Int, Int, Int] {
      override def runBazaar: RunBazaar[* => *, Int, Int, Int, Int] = new RunBazaar[* => *, Int, Int, Int, Int] {
        override def apply[F[_]](pafb: Int => F[Int])(s: Int)(implicit ev: Applicative[F]): F[Int] = pafb(s)
      }
    })
  }

  implicit def eqBazaar0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, Int, Int]] = Eq.instance[Bazaar[* => *, Int, Int, Int, Int]] { (bazaar1, bazaar2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      bazaar1.runBazaar.apply[Id](identity)(int) === bazaar2.runBazaar.apply[Id](identity)(int)
    }
  }

  implicit def eqBazaar1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, (Int, Int), Int]] = Eq.instance[Bazaar[* => *, Int, Int, (Int, Int), Int]] {
    (bazaar1, bazaar2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val pair = (int, int)

        bazaar1.runBazaar.apply[Id](identity)(pair) === bazaar2.runBazaar.apply[Id](identity)(pair)
      }
  }

  implicit def eqBazaar2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, (Int, Int), (Int, Int)]] =
    Eq.instance[Bazaar[* => *, Int, Int, (Int, Int), (Int, Int)]] { (bazaar1, bazaar2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val pair = (int, int)

        bazaar1.runBazaar.apply[Id](identity)(pair) === bazaar2.runBazaar.apply[Id](identity)(pair)
      }
    }

  implicit def eqBazaar3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, (Int, (Int, Int)), (Int, (Int, Int))]] =
    Eq.instance[Bazaar[* => *, Int, Int, (Int, (Int, Int)), (Int, (Int, Int))]] { (bazaar1, bazaar2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val triple = (int, (int, int))

        bazaar1.runBazaar.apply[Id](identity)(triple) === bazaar2.runBazaar.apply[Id](identity)(triple)
      }
    }

  implicit def eqBazaar4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, ((Int, Int), Int), ((Int, Int), Int)]] =
    Eq.instance[Bazaar[* => *, Int, Int, ((Int, Int), Int), ((Int, Int), Int)]] { (bazaar1, bazaar2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val triple = ((int, int), int)

        bazaar1.runBazaar.apply[Id](identity)(triple) === bazaar2.runBazaar.apply[Id](identity)(triple)
      }
    }

  implicit def eqBazaar5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, Int, Either[Int, Int]]] =
    Eq.instance[Bazaar[* => *, Int, Int, Int, Either[Int, Int]]] { (bazaar1, bazaar2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        bazaar1.runBazaar.apply[Id](identity)(int) === bazaar2.runBazaar.apply[Id](identity)(int)
      }
    }

  implicit def eqBazaar6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, Either[Int, Int], Either[Int, Int]]] =
    Eq.instance[Bazaar[* => *, Int, Int, Either[Int, Int], Either[Int, Int]]] { (bazaar1, bazaar2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Int]

        bazaar1.runBazaar.apply[Id](identity)(either) === bazaar2.runBazaar.apply[Id](identity)(either)
      }
    }

  implicit def eqBazaar7(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Eq.instance[Bazaar[* => *, Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] { (bazaar1, bazaar2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Either[Int, Int]]

        bazaar1.runBazaar.apply[Id](identity)(either) === bazaar2.runBazaar.apply[Id](identity)(either)
      }
    }

  implicit def eqBazaar8(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Bazaar[* => *, Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Eq.instance[Bazaar[* => *, Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] { (bazaar1, bazaar2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asLeft[Either[Int, Int]]

        bazaar1.runBazaar.apply[Id](identity)(either) === bazaar2.runBazaar.apply[Id](identity)(either)
      }
    }

  checkAll("Profunctor Bazaar[* => *, Int, Int, Int, Int]", ProfunctorTests[Bazaar[* => *, Int, Int, *, *]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Strong Bazaar[* => *, Int, Int, Int, Int]", StrongTests[Bazaar[* => *, Int, Int, *, *]].strong[Int, Int, Int, Int, Int, Int])
  checkAll("Choice Bazaar[* => *, Int, Int, Int, Int]", ChoiceTests[Bazaar[* => *, Int, Int, *, *]].choice[Int, Int, Int, Int, Int, Int])
  checkAll("Wander Bazaar[* => *, Int, Int, Int, Int]", WanderTests[Bazaar[* => *, Int, Int, *, *]].wander[Int, Int, Int, Int, Int, Int])
}
