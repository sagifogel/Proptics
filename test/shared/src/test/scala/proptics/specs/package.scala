package proptics

import scala.Function.const
import scala.collection.immutable.ListMap

import cats.arrow.Strong
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector, OneAnd, State}
import cats.kernel.Eq
import cats.syntax.bifunctor._
import cats.syntax.eq._
import org.scalacheck.{Arbitrary, Cogen, Gen}

import proptics.data.Disj
import proptics.internal.Forget
import proptics.profunctor.Star
import proptics.syntax.star._

package object specs {
  final case class Person(name: String, address: Address)
  final case class Address(city: String, street: Street)
  final case class Street(name: String, number: Int)
  val emptyStr = ""
  val whole9: Whole = Whole(9)
  val listEmpty: List[Int] = Nil
  val emptyList: List[Int] = List.empty[Int]
  val list: List[Int] = List(1, 2, 3, 4, 5, 6)
  val jNumber: JNumber = JNumber(9d)
  val jsonContent: String = "proptics"
  val jStrEmpty: JString = JString("")
  val greaterThan5: Int => Boolean = _ > 5
  val greaterThan10: Int => Boolean = _ > 10
  def oneToNine: ((Int, Int)) => Int = _._1 + 8
  val evenNumbers: Int => Boolean = _ % 2 === 0
  val jStringContent: JString = JString(jsonContent)
  val indexedList: List[(Int, Int)] = list.zipWithIndex
  val boolList: List[Boolean] = List(true, false, true, false)
  val falseBoolList: List[Boolean] = boolList.map(const(false))
  val nel: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(list)
  val emptyIndexedList: List[(Int, Int)] = List.empty[(Int, Int)]
  implicit val eqPerson: Eq[Person] = Eq.fromUniversalEquals[Person]
  val jStringContentUppercase: JString = JString(jsonContent.toUpperCase)
  def lengthGreaterThan5(str: String): Boolean = greaterThan5(str.length)
  def lengthGreaterThan10(str: String): Boolean = greaterThan10(str.length)
  val nelBool: NonEmptyList[Boolean] = NonEmptyList.fromListUnsafe(boolList)
  implicit val eqPairOfIns: Eq[(Int, Int)] = Eq.fromUniversalEquals[(Int, Int)]
  val nelFalseBool: NonEmptyList[Boolean] = NonEmptyList.fromListUnsafe(falseBoolList)
  implicit val eqArray: Eq[Array[Int]] = Eq.instance[Array[Int]](_.toList === _.toList)
  implicit val state: State[NonEmptyList[Int], Int] = State.pure[NonEmptyList[Int], Int](1)
  implicit val eqPairOfIntAndOption: Eq[(Int, Option[Int])] = Eq.fromUniversalEquals[(Int, Option[Int])]
  implicit val nelState: State[NonEmptyList[(Int, Int)], Int] = State.pure[NonEmptyList[(Int, Int)], Int](1)

  implicit val eqListMap: Eq[ListMap[Int, Int]] = Eq.instance[ListMap[Int, Int]] { (lsm1, lsm2) =>
    lsm1.foldLeft(true) { case (b, (key, value)) =>
      b && lsm2.get(key).fold(false)(_ === value)
    }
  }

  implicit val arbForget: Arbitrary[Forget[Int, Int, Int]] = Arbitrary[Forget[Int, Int, Int]] {
    for {
      runForget <- Gen.function1[Int, Int](Arbitrary.arbInt.arbitrary)
    } yield Forget(runForget)
  }

  implicit val arbForget2Forget: Arbitrary[Forget[Int, Int, Int] => Forget[Int, Int, Int]] = Arbitrary[Forget[Int, Int, Int] => Forget[Int, Int, Int]] {
    for {
      runForget <- Gen.function1[Forget[Int, Int, Int], Forget[Int, Int, Int]](arbForget.arbitrary)
    } yield runForget
  }

  implicit def cogenForget: Cogen[Forget[Int, Int, Int]] = Cogen.function1[Int, Int].contramap(_.runForget)

  implicit val arbChain: Arbitrary[Chain[Int]] = Arbitrary[Chain[Int]] {
    for {
      list <- Gen.listOf(Arbitrary.arbInt.arbitrary)
    } yield Chain(list: _*)
  }

  implicit val arbNev: Arbitrary[NonEmptyVector[Int]] = Arbitrary[NonEmptyVector[Int]] {
    for {
      first <- Arbitrary.arbInt.arbitrary
      rest <- Gen.listOf(Arbitrary.arbInt.arbitrary)
    } yield NonEmptyVector(first, rest.toVector)
  }

  implicit val arbNel: Arbitrary[NonEmptyList[Int]] = Arbitrary[NonEmptyList[Int]] {
    for {
      first <- Arbitrary.arbInt.arbitrary
      rest <- Gen.listOf(Arbitrary.arbInt.arbitrary)
    } yield NonEmptyList(first, rest)
  }

  implicit val arbNes: Arbitrary[NonEmptySet[Int]] = Arbitrary[NonEmptySet[Int]] {
    for {
      first <- Arbitrary.arbInt.arbitrary
      rest <- Gen.listOf(Arbitrary.arbInt.arbitrary)
    } yield NonEmptySet(first, collection.immutable.SortedSet(rest: _*))
  }

  implicit val arbOneAnd: Arbitrary[OneAnd[List, Int]] = Arbitrary[OneAnd[List, Int]] {
    for {
      first <- Arbitrary.arbInt.arbitrary
      rest <- Gen.listOf(Arbitrary.arbInt.arbitrary)
    } yield OneAnd(first, rest)
  }

  implicit val arbPair: Arbitrary[(Int, Int)] = Arbitrary[(Int, Int)] {
    for {
      int1 <- Arbitrary.arbInt.arbitrary
      int2 <- Arbitrary.arbInt.arbitrary
    } yield (int1, int2)
  }

  implicit val arbNem: Arbitrary[NonEmptyMap[Int, Int]] = Arbitrary[NonEmptyMap[Int, Int]] {
    for {
      ls <- Gen.nonEmptyListOf(arbPair.arbitrary)
      sorted = collection.immutable.SortedMap(ls: _*)
    } yield NonEmptyMap(ls.head, sorted)
  }

  implicit val arbNonEmptyChain: Arbitrary[NonEmptyChain[Int]] = Arbitrary[NonEmptyChain[Int]] {
    for {
      head <- Arbitrary.arbInt.arbitrary
      list <- Gen.listOf(Arbitrary.arbInt.arbitrary)
    } yield NonEmptyChain(head, list: _*)
  }

  implicit val arbStreet: Arbitrary[Street] = Arbitrary[Street] {
    for {
      name <- Arbitrary.arbString.arbitrary
      number <- Arbitrary.arbInt.arbitrary
    } yield Street(name, number)
  }

  implicit val arbAddress: Arbitrary[Address] = Arbitrary[Address] {
    for {
      street <- arbStreet.arbitrary
      city <- Arbitrary.arbString.arbitrary
    } yield Address(city, street)
  }

  implicit val arbPerson: Arbitrary[Person] = Arbitrary[Person] {
    for {
      name <- Arbitrary.arbString.arbitrary
      address <- arbAddress.arbitrary
    } yield Person(name, address)
  }

  implicit val cogenChain: Cogen[Chain[Int]] = Cogen.it(_.iterator)

  implicit def strongStarTupleOfDisj: Strong[Star[(Disj[Boolean], *), *, *]] = new Strong[Star[(Disj[Boolean], *), *, *]] {
    override def first[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (A, C), (B, C)] =
      Star { case (a, c) =>
        fa.runStar(a).bimap(identity, (_, c))
      }

    override def second[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (C, A), (C, B)] =
      Star { case (c, a) =>
        fa.runStar(a).bimap(identity, (c, _))
      }

    override def dimap[A, B, C, D](fab: Star[(Disj[Boolean], *), A, B])(f: C => A)(g: B => D): Star[(Disj[Boolean], *), C, D] =
      Star(c => fab.runStar(f(c)).bimap(identity, g))
  }

  implicit def strongStarTupleOfNegativeDisj: Strong[Star[(Disj[Boolean], *), *, *]] = new Strong[Star[(Disj[Boolean], *), *, *]] {
    override def first[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (A, C), (B, C)] =
      Star { case (a, c) =>
        fa.runStar(a).bimap(const(Disj(false)), (_, c))
      }

    override def second[A, B, C](fa: Star[(Disj[Boolean], *), A, B]): Star[(Disj[Boolean], *), (C, A), (C, B)] =
      Star { case (c, a) =>
        fa.runStar(a).bimap(const(Disj(false)), (c, _))
      }

    override def dimap[A, B, C, D](fab: Star[(Disj[Boolean], *), A, B])(f: C => A)(g: B => D): Star[(Disj[Boolean], *), C, D] =
      Star(c => fab.runStar(f(c)).bimap(const(Disj(false)), g))
  }
}
