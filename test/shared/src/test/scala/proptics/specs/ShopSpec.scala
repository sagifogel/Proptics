package proptics.specs

import cats.Eq
import cats.laws.discipline.{ExhaustiveCheck, MiniInt, ProfunctorTests, StrongTests}
import proptics.internal.Shop
import org.scalacheck.ScalacheckShapeless._

class ShopSpec extends PropticsSuite {
  implicit def eqShop0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Shop[Int, Int, Int, Int]] = Eq.instance[Shop[Int, Int, Int, Int]] { (shop1, shop2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      shop1.get(int) === shop2.get(int) && shop1.set(int)(int) === shop2.set(int)(int)
    }
  }
  implicit def eqShop1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Shop[Int, Int, (Int, Int), Int]] = Eq.instance[Shop[Int, Int, (Int, Int), Int]] { (shop1, shop2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      shop1.get((int, int)) === shop2.get((int, int)) && shop1.set((int, int))(int) === shop2.set((int, int))(int)
    }
  }

  implicit def eqShop2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Shop[Int, Int, (Int, Int), (Int, Int)]] =
    Eq.instance[Shop[Int, Int, (Int, Int), (Int, Int)]] { (shop1, shop2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        shop1.get((int, int)) === shop2.get((int, int)) && shop1.set((int, int))(int) === shop2.set((int, int))(int)
      }
    }

  implicit def eqShop3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Shop[Int, Int, ((Int, Int), Int), (Int, Int)]] =
    Eq.instance[Shop[Int, Int, ((Int, Int), Int), (Int, Int)]] { (shop1, shop2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        shop1.get(((int, int), int)) === shop2.get(((int, int), int)) && shop1.set(((int, int), int))(int) === shop2.set(((int, int), int))(int)
      }
    }

  implicit def eqShop4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Shop[Int, Int, (Int, (Int, Int)), (Int, (Int, Int))]] =
    Eq.instance[Shop[Int, Int, (Int, (Int, Int)), (Int, (Int, Int))]] { (shop1, shop2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        shop1.get((int, (int, int))) === shop2.get((int, (int, int))) && shop1.set((int, (int, int)))(int) === shop2.set((int, (int, int)))(int)

      }
    }

  implicit def eqShop5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Shop[Int, Int, ((Int, Int), Int), ((Int, Int), Int)]] =
    Eq.instance[Shop[Int, Int, ((Int, Int), Int), ((Int, Int), Int)]] { (shop1, shop2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt

        shop1.get(((int, int), int)) === shop2.get(((int, int), int)) && shop1.set(((int, int), int))(int) === shop2.set(((int, int), int))(int)

      }
    }

  checkAll("Profunctor Shop[Int, Int, Int, Int]", ProfunctorTests[Shop[Int, Int, *, *]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Strong Shop[Int, Int, Int, Int]", StrongTests[Shop[Int, Int, *, *]].strong[Int, Int, Int, Int, Int, Int])
}
