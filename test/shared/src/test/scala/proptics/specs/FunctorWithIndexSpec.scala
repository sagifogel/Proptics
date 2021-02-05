package proptics.specs

import cats.data.NonEmptyList
import org.scalacheck.ScalacheckShapeless._

import proptics.instances.functorWithIndex._
import proptics.law.discipline.FunctorWithIndexTests

class FunctorWithIndexSpec extends PropticsSuite {
  checkAll("FunctorWithIndex[Option, Unit]", FunctorWithIndexTests[Option, Unit].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndex[List, Int]", FunctorWithIndexTests[List, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndex[NonEmptyList, Int]", FunctorWithIndexTests[NonEmptyList, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndex[Map[Int, *], Int]", FunctorWithIndexTests[Map[Int, *], Int].functorWithIndex[Int, Int, Int])
}
