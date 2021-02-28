package proptics.specs

import scala.collection.immutable.{ListMap, SortedMap}

import cats.Eq
import cats.laws.discipline.{ExhaustiveCheck, MiniInt}

import proptics.instances.index._
import proptics.law.discipline.IndexTests

private[specs] trait IndexSpec0 extends PropticsSuite {
  implicit val eqListMap: Eq[ListMap[Int, Int]] = Eq.fromUniversalEquals[ListMap[Int, Int]]
  implicit def eqArrow(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => Int] = Eq.instance[Int => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int) === f2(int)
    }
  }

  checkAll("Index[Int => Int]", IndexTests[Int => Int, Int, Int].index)
  checkAll("Index[Option[Int]]", IndexTests[Option[Int], Unit, Int].index)
  checkAll("Index[Array[Int]]", IndexTests[Array[Int], Int, Int].index)
  checkAll("Index[Vector[Int]]", IndexTests[Vector[Int], Int, Int].index)
  checkAll("Index[List[Int]]", IndexTests[List[Int], Int, Int].index)
  checkAll("Index[Set[Int]]", IndexTests[Set[Int], Int, Unit].index)
  checkAll("Index[SortedMap[Int, Int]]", IndexTests[SortedMap[Int, Int], Int, Int].index)
  checkAll("Index[ListMap[Int, Int]]", IndexTests[ListMap[Int, Int], Int, Int].index)
  checkAll("Index[Map[Int, Int]]", IndexTests[Map[Int, Int], Int, Int].index)
}
