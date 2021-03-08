package proptics.specs

import scala.collection.immutable.ListMap

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptyVector, OneAnd}
import cats.instances.int._
import org.scalacheck.ScalacheckShapeless._

import proptics.instances.functorWithIndex._
import proptics.law.discipline.FunctorWithIndexTests

private[specs] trait FunctorWithIndexSpec0 extends PropticsSuite {
  checkAll("FunctorWithIndexSpec[Option, Unit]", FunctorWithIndexTests[Option, Unit].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[Vector, Int]", FunctorWithIndexTests[Vector, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[List, Int]", FunctorWithIndexTests[List, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[ListMap[Int, *], Int]", FunctorWithIndexTests[ListMap[Int, *], Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[Map[Int, *], Int]", FunctorWithIndexTests[Map[Int, *], Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[Chain, Int]", FunctorWithIndexTests[Chain, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[NonEmptyVector, Int]", FunctorWithIndexTests[NonEmptyVector, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[NonEmptyList, Int]", FunctorWithIndexTests[NonEmptyList, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[NonEmptyChain, Int]", FunctorWithIndexTests[NonEmptyChain, Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[OneAnd, Int]", FunctorWithIndexTests[OneAnd[List, *], Int].functorWithIndex[Int, Int, Int])
  checkAll("FunctorWithIndexSpec[NonEmptyMap, Int]", FunctorWithIndexTests[NonEmptyMap[Int, *], Int].functorWithIndex[Int, Int, Int])
}
