package proptics.specs

import scala.collection.immutable.{ListMap, SortedMap}

import cats.Eq
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
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
  checkAll("Index[ListMap[Int, Int]]", IndexTests[ListMap[Int, Int], Int, Int].index)
  checkAll("Index[Set[Int]]", IndexTests[Set[Int], Int, Unit].index)
  checkAll("Index[SortedMap[Int, Int]]", IndexTests[SortedMap[Int, Int], Int, Int].index)
  checkAll("Index[Map[Int, Int]]", IndexTests[Map[Int, Int], Int, Int].index)
  checkAll("Index[Chain[Int]]", IndexTests[Chain[Int], Int, Int].index)
  checkAll("Index[NonEmptyVector[Int]]", IndexTests[NonEmptyVector[Int], Int, Int].index)
  checkAll("Index[NonEmptyList[Int]]", IndexTests[NonEmptyList[Int], Int, Int].index)
  checkAll("Index[NonEmptySet[Int]]", IndexTests[NonEmptySet[Int], Int, Unit].index)
  checkAll("Index[NonEmptyMap[Int, Int]]", IndexTests[NonEmptyMap[Int, Int], Int, Int].index)
  checkAll("Index[NonEmptyChain[Int]]", IndexTests[NonEmptyChain[Int], Int, Int].index)
}
