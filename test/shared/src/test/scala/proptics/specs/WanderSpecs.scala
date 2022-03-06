package proptics.specs

import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.syntax.either._
import cats.{Eq, Id}
import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Cogen.cogenInt
import org.scalacheck.{Arbitrary, Gen}

import proptics.law.discipline.WanderTests
import proptics.profunctor.{Star, Wander}
import proptics.syntax.star._

class WanderSpecs extends PropticsSuite {
  implicit val wanderStar: Wander[Star[Id, *, *]] = Wander.wanderStar[Id]
  implicit val arbIntInt: Arbitrary[Int => Int] = Arbitrary.arbFunction1[Int, Int]

  implicit def arbStarId: Arbitrary[Star[Id, Int, Int]] = Arbitrary[Star[Id, Int, Int]] {
    Gen.const[Star[Id, Int, Int]](Star[Id, Int, Int](identity))
  }

  implicit def eqWanderFn0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => Int] = Eq.instance[Int => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int) === f2(int)
    }
  }

  implicit def eqWanderFn1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[((Int, Int)) => Int] = Eq.instance[((Int, Int)) => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1((int, int)) === f2((int, int))
    }
  }

  implicit def eqWanderFn2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[((Int, Int)) => (Int, Int)] =
    Eq.instance[((Int, Int)) => (Int, Int)] { (f1, f2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        f1((int, int)) === f2((int, int))
      }
    }

  implicit def eqWanderFn3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[(((Int, Int), Int)) => ((Int, Int), Int)] =
    Eq.instance[(((Int, Int), Int)) => ((Int, Int), Int)] { (f1, f2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        f1(((int, int), int)) === f2(((int, int), int))
      }
    }

  implicit def eqWanderFn4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[((Int, (Int, Int))) => (Int, (Int, Int))] =
    Eq.instance[((Int, (Int, Int))) => (Int, (Int, Int))] { (f1, f2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        f1((int, (int, int))) === f2((int, (int, int)))
      }
    }

  implicit def eqWanderFn5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => Either[Int, Int]] =
    Eq.instance[Int => Either[Int, Int]] { (f1, f2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        f1(int) === f2(int)
      }
    }

  implicit def eqWanderFn6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Either[Int, Int] => Either[Int, Int]] =
    Eq.instance[Either[Int, Int] => Either[Int, Int]] { (f1, f2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Int]

        f1(either) === f2(either)
      }
    }

  implicit def eqWanderFn7(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Either[Either[Int, Int], Int] => Either[Either[Int, Int], Int]] =
    Eq.instance[Either[Either[Int, Int], Int] => Either[Either[Int, Int], Int]] { (f1, f2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Either[Int, Int]]

        f1(either) === f2(either)
      }
    }

  implicit def eqWanderFn8(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Either[Int, Either[Int, Int]] => Either[Int, Either[Int, Int]]] =
    Eq.instance[Either[Int, Either[Int, Int]] => Either[Int, Either[Int, Int]]] { (f1, f2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asLeft[Either[Int, Int]]

        f1(either) === f2(either)
      }
    }

  implicit def eqWanderStar0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, Int, Int]] = Eq.instance[Star[Id, Int, Int]] { (star1, star2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      star1.runStar(int) === star2.runStar(int)
    }
  }

  implicit def eqWanderStar1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, (Int, Int), Int]] = Eq.instance[Star[Id, (Int, Int), Int]] { (star1, star2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      star1.runStar((int, int)) === star2.runStar((int, int))
    }
  }

  implicit def eqWanderStar2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, (Int, Int), (Int, Int)]] = Eq.instance[Star[Id, (Int, Int), (Int, Int)]] { (star1, star2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      star1.runStar((int, int)) === star2.runStar((int, int))
    }
  }

  implicit def eqWanderStar3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, ((Int, Int), Int), ((Int, Int), Int)]] =
    Eq.instance[Star[Id, ((Int, Int), Int), ((Int, Int), Int)]] { (star1, star2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        star1.runStar(((int, int), int)) === star2.runStar(((int, int), int))
      }
    }

  implicit def eqWanderStar4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, (Int, (Int, Int)), (Int, (Int, Int))]] =
    Eq.instance[Star[Id, (Int, (Int, Int)), (Int, (Int, Int))]] { (star1, star2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        star1.runStar((int, (int, int))) === star2.runStar((int, (int, int)))
      }
    }

  implicit def eqWanderStar5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, Int, Either[Int, Int]]] =
    Eq.instance[Star[Id, Int, Either[Int, Int]]] { (star1, star2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        star1.runStar(int) === star2.runStar(int)
      }
    }

  implicit def eqWanderStar6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, Either[Int, Int], Either[Int, Int]]] =
    Eq.instance[Star[Id, Either[Int, Int], Either[Int, Int]]] { (star1, star2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Int]

        star1.runStar(either) === star2.runStar(either)
      }
    }

  implicit def eqWanderStar7(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Eq.instance[Star[Id, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] { (star1, star2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asRight[Either[Int, Int]]

        star1.runStar(either) === star2.runStar(either)
      }
    }

  implicit def eqWanderStar8(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Star[Id, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Eq.instance[Star[Id, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] { (star1, star2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val either = int.asLeft[Either[Int, Int]]

        star1.runStar(either) === star2.runStar(either)
      }
    }

  checkAll("Wander * => *", WanderTests[* => *].wander[Int, Int, Int, Int, Int, Int])
  checkAll("Wander Star[Id, Int, Int]", WanderTests[Star[Id, *, *]].wander[Int, Int, Int, Int, Int, Int])
}
