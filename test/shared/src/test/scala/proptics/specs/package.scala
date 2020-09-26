package proptics

import cats.arrow.Strong
import cats.data.{NonEmptyList, State}
import cats.kernel.Eq
import cats.syntax.eq._
import cats.syntax.bifunctor._
import org.scalacheck.{Arbitrary, Gen}
import proptics.newtype.Disj
import proptics.profunctor.Star
import proptics.syntax.star._

import scala.Function.const

package object specs {
  val emptyStr = ""
  val whole9: Whole = Whole(9)
  val emptyList = List.empty[Int]
  val list = List(1, 2, 3, 4, 5, 6)
  val jNumber: JNumber = JNumber(9d)
  val jsonContent: String = "proptics"
  val jStrEmpty: JString = JString("")
  val foldState: FoldState = FoldState(1)
  val greaterThan5: Int => Boolean = _ > 5
  val greaterThan10: Int => Boolean = _ > 10
  def oneToNine: ((Int, Int)) => Int = _._2 + 8
  val evenNumbers: Int => Boolean = _ % 2 === 0
  val jStringContent: JString = JString(jsonContent)
  val boolList: List[Boolean] = List(true, false, true, false)
  val falseBoolList: List[Boolean] = boolList.map(const(false))
  val nel: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(list)
  val emptyIndexedList: List[(Int, Int)] = List.empty[(Int, Int)]
  val indexedList: List[(Int, Int)] = list.zipWithIndex.map(_.swap)
  val jStringContentUppercase: JString = JString(jsonContent.toUpperCase)
  def lengthGreaterThan5(str: String): Boolean = greaterThan5(str.length)
  def lengthGreaterThan10(str: String): Boolean = greaterThan10(str.length)
  implicit val eqPairOfIns: Eq[(Int, Int)] = Eq.fromUniversalEquals[(Int, Int)]
  implicit val state: State[NonEmptyList[Int], Int] = State.pure[NonEmptyList[Int], Int](1)
  implicit val eqPairOfIntAndOption: Eq[(Int, Option[Int])] = Eq.fromUniversalEquals[(Int, Option[Int])]
  implicit val nelState: State[NonEmptyList[(Int, Int)], Int] = State.pure[NonEmptyList[(Int, Int)], Int](1)
  implicit def eqPair[I: Eq, A: Eq]: Eq[(I, A)] = new Eq[(I, A)] {
    override def eqv(x: (I, A), y: (I, A)): Boolean = x._1 === y._1 && x._2 === y._2
  }

  implicit def eqListOfPairs[I: Eq, A: Eq]: Eq[List[(I, A)]] = new Eq[List[(I, A)]] {
    override def eqv(x: List[(I, A)], y: List[(I, A)]): Boolean =
      x.length === y.length && x.zip(y).forall { case (a, b) => eqPair[I, A].eqv(a, b) }
  }

  implicit val arbNel: Arbitrary[NonEmptyList[Int]] = Arbitrary[NonEmptyList[Int]] {
    for {
      first <- Arbitrary.arbInt.arbitrary
      rest <- Gen.nonEmptyListOf(Arbitrary.arbInt.arbitrary)
    } yield NonEmptyList(first, rest)
  }

  implicit def strongStarTupleOfDisj: Strong[Star[(Disj[Boolean], *), *, *]] = new Strong[Star[(Disj[Boolean], *), *, *]] {
    override def first[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (A, C), (B, C)] =
      Star {
        case (a, c) =>
          fa.runStar(a).bimap(identity, (_, c))
      }

    override def second[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (C, A), (C, B)] =
      Star {
        case (c, a) =>
          fa.runStar(a).bimap(identity, (c, _))
      }

    override def dimap[A, B, C, D](fab: Star[(Disj[Boolean], *), A, B])(f: C => A)(g: B => D): Star[(Disj[Boolean], *), C, D] =
      Star(c => fab.runStar(f(c)).bimap(identity, g))
  }

  implicit def strongStarTupleOfNegativeDisj: Strong[Star[(Disj[Boolean], *), *, *]] = new Strong[Star[(Disj[Boolean], *), *, *]] {
    override def first[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (A, C), (B, C)] =
      Star {
        case (a, c) =>
          fa.runStar(a).bimap(const(Disj(false)), (_, c))
      }

    override def second[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (C, A), (C, B)] =
      Star {
        case (c, a) =>
          fa.runStar(a).bimap(const(Disj(false)), (c, _))
      }

    override def dimap[A, B, C, D](fab: Star[(Disj[Boolean], *), A, B])(f: C => A)(g: B => D): Star[(Disj[Boolean], *), C, D] =
      Star(c => fab.runStar(f(c)).bimap(const(Disj(false)), g))
  }
}
