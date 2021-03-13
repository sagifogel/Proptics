package proptics.specs

import cats.data.Chain

import proptics.instances.cons._
import proptics.law.discipline.ConsTests

private[specs] trait ConsSpec0 extends PropticsSuite {
  checkAll("Cons[String, Char] cons", ConsTests[String, Char].cons)
  checkAll("Cons[String, Char] headOption", ConsTests[String, Char].headOption)
  checkAll("Cons[String, Char] tailOption", ConsTests[String, Char].tailOption)

  checkAll("Cons[List[Int], Int] cons", ConsTests[List[Int], Int].cons)
  checkAll("Cons[List[Int], Int] headOption", ConsTests[List[Int], Int].headOption)
  checkAll("Cons[List[Int], Int] tailOption", ConsTests[List[Int], Int].tailOption)

  checkAll("Cons[Array[Int], Int] cons", ConsTests[Array[Int], Int].cons)
  checkAll("Cons[Array[Int], Int] headOption", ConsTests[Array[Int], Int].headOption)
  checkAll("Cons[Array[Int], Int] tailOption", ConsTests[Array[Int], Int].tailOption)

  checkAll("Cons[Vector[Int], Int] cons", ConsTests[Vector[Int], Int].cons)
  checkAll("Cons[Vector[Int], Int] headOption", ConsTests[Vector[Int], Int].headOption)
  checkAll("Cons[Vector[Int], Int] tailOption", ConsTests[Vector[Int], Int].tailOption)

  checkAll("Cons[Chain[Int], Int] cons", ConsTests[Chain[Int], Int].cons)
  checkAll("Cons[Chain[Int], Int] headOption", ConsTests[Chain[Int], Int].headOption)
  checkAll("Cons[Chain[Int], Int] tailOption", ConsTests[Chain[Int], Int].tailOption)
}
