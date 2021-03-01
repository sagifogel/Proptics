package proptics.specs

import cats.Eq
import cats.instances.int._
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import org.scalacheck.ScalacheckShapeless._
import proptics.instances.functorWithIndex._
import proptics.law.discipline.FunctorWithIndexTests

import scala.collection.immutable.ListMap

private[specs] trait FunctorWithIndexSpec0 extends PropticsSuite {
  implicit val eqListMap: Eq[ListMap[Int, Int]] = Eq.instance[ListMap[Int, Int]] { (lsm1, lsm2) =>
    lsm1.foldLeft(true) { case (b, (key, value)) =>
      b && lsm2.get(key).fold(false)(_ === value)
    }
  }

  checkAll("FunctorWithIndexSpec[Option, Unit]", FunctorWithIndexTests[Option, Unit].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[Vector, Int]", FunctorWithIndexTests[Vector, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[List, Int]", FunctorWithIndexTests[List, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[ListMap[Int, *], Int]", FunctorWithIndexTests[ListMap[Int, *], Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[Map[Int, *], Int]", FunctorWithIndexTests[Map[Int, *], Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[NonEmptyVector, Int]", FunctorWithIndexTests[NonEmptyVector, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[NonEmptyList, Int]", FunctorWithIndexTests[NonEmptyList, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[NonEmptyMap, Int]", FunctorWithIndexTests[NonEmptyMap[Int, *], Int].functorWithIndex[Int, Int, Int])
}
