package proptics.specs

import scala.Function.const
import scala.util.Random

import cats.data.State
import cats.syntax.option._
import spire.std.boolean._
import spire.std.int._

import proptics._
import proptics.syntax.applied.fold._

class AppliedFoldSpec extends PropticsSuite {
  val ones: List[Int] = List.fill(10)(1)
  val listFoldable: AppliedFold[List[Int], Int] = list.foldable
  val emptyFoldable: AppliedFold[List[Int], Int] = emptyList.foldable
  val negativeBoolFoldable: AppliedFold[List[Boolean], Boolean] = falseBoolList.foldable
  val stringListFoldable: AppliedFold[List[String], String] = List("Hello", "World!").foldable
  val positiveBoolFoldable: AppliedFold[List[Boolean], Boolean] = boolList.map(const(true)).foldable

  test("viewAll") {
    listFoldable.viewAll shouldEqual list
    emptyFoldable.viewAll shouldEqual emptyList
  }

  test("preview") {
    listFoldable.preview shouldEqual 1.some
    emptyFoldable.preview shouldEqual None
  }

  test("foldMap") {
    listFoldable.foldMap(identity) shouldEqual list.sum
    listFoldable.foldMap(List(_)) shouldEqual list
    emptyFoldable.foldMap(identity) shouldEqual 0
    emptyFoldable.foldMap(List(_)) shouldEqual emptyList
  }

  test("fold") {
    listFoldable.fold shouldEqual list.sum
    listFoldable.fold shouldEqual listFoldable.view
    emptyFoldable.fold shouldEqual 0
    emptyFoldable.fold shouldEqual emptyFoldable.view
  }

  test("foldRight") {
    listFoldable.foldRight(0)(_ + _) shouldEqual list.sum
    listFoldable.foldRight(0)(_ + _) should be > 0
    listFoldable.foldRight(emptyList)(_ :: _) shouldEqual list
    emptyFoldable.foldRight(0)(_ + _) shouldEqual 0
    emptyFoldable.foldRight(0)(_ - _) shouldEqual 0

  }

  test("foldLeft") {
    listFoldable.foldLeft(0)(_ + _) shouldEqual list.sum
    listFoldable.foldLeft(0)(_ + _) should be > 0
    listFoldable.foldLeft(emptyList)((ls, a) => a :: ls) shouldEqual list.reverse
    emptyFoldable.foldLeft(0)(_ + _) shouldEqual 0
    emptyFoldable.foldLeft(0)(_ - _) shouldEqual 0
  }

  test("isEmpty") {
    listFoldable.isEmpty shouldEqual false
    emptyFoldable.isEmpty shouldEqual true
  }

  test("nonEmpty") {
    listFoldable.nonEmpty shouldEqual true
    emptyFoldable.nonEmpty shouldEqual false
    listFoldable.nonEmpty shouldEqual !listFoldable.isEmpty
  }

  test("length") {
    listFoldable.length shouldEqual list.length
    emptyFoldable.length shouldEqual 0
  }

  test("find") {
    listFoldable.find(greaterThan5) shouldEqual list.find(greaterThan5)
    emptyFoldable.find(greaterThan10) shouldEqual None
  }

  test("first") {
    listFoldable.first shouldEqual list.head.some
    emptyFoldable.first shouldEqual None
  }

  test("last") {
    listFoldable.last shouldEqual list.last.some
    emptyFoldable.last shouldEqual None
  }

  test("minimum") {
    Random.shuffle(list).foldable.minimum shouldEqual list.head.some
    emptyFoldable.minimum shouldEqual None
  }

  test("maximum") {
    Random.shuffle(list).foldable.maximum shouldEqual list.last.some
    emptyFoldable.maximum shouldEqual None
  }

  test("toArray") {
    list.foldable.toArray shouldEqual list.toArray
  }

  test("toList") {
    listFoldable.toList shouldEqual list
  }

  test("sum") {
    listFoldable.sum shouldEqual list.sum
  }

  test("product") {
    listFoldable.product shouldEqual list.product
    emptyFoldable.product shouldEqual 1
  }

  test("forall") {
    listFoldable.forall(_ < 10) shouldEqual true
    emptyFoldable.forall(_ < 10) shouldEqual true
    listFoldable.forall(_ > 10) shouldEqual false
    emptyFoldable.forall(_ > 10) shouldEqual true
  }

  test("forall using heyting") {
    listFoldable.forallH(_ < 10) shouldEqual true
    emptyFoldable.forallH(_ < 10) shouldEqual true
    listFoldable.forallH(_ > 10) shouldEqual false
    emptyFoldable.forallH(_ > 10) shouldEqual true
  }

  test("and") {
    boolList.foldable_[Boolean].and shouldEqual false
    positiveBoolFoldable.and shouldEqual true
    negativeBoolFoldable.and shouldEqual false
  }

  test("or") {
    positiveBoolFoldable.or shouldEqual true
    negativeBoolFoldable.or shouldEqual false
  }

  test("any") {
    listFoldable.any(greaterThan5) shouldEqual true
    listFoldable.any(greaterThan10) shouldEqual false
  }

  test("exist") {
    listFoldable.exists(greaterThan5) shouldEqual true
    listFoldable.exists(greaterThan10) shouldEqual false
  }

  test("notExists") {
    listFoldable.notExists(greaterThan5) shouldEqual false
    listFoldable.notExists(greaterThan10) shouldEqual true
    listFoldable.notExists(greaterThan10) shouldEqual !listFoldable.exists(greaterThan10)
  }

  test("contains") {
    listFoldable.contains(5) shouldEqual true
    listFoldable.contains(10) shouldEqual false
  }

  test("notContains") {
    listFoldable.notContains(5) shouldEqual false
    listFoldable.notContains(10) shouldEqual true
    listFoldable.notContains(10) shouldEqual !listFoldable.contains(10)
  }

  test("use") {
    implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](1)

    listFoldable.use.runA(list).value shouldEqual list
  }

  test("filterByIndex") {
    listFoldable.filterByIndex(_ < 3).viewAll shouldEqual list.take(3)
  }

  test("single") {
    listFoldable.single(1).viewAll shouldEqual List(2)
  }

  test("take") {
    listFoldable.take(3).viewAll shouldEqual List(1, 2, 3)
  }

  test("drop") {
    listFoldable.drop(3).viewAll shouldEqual List(4, 5, 6)
  }

  test("takeWhile") {
    listFoldable.takeWhile(_ < 4).viewAll shouldEqual List(1, 2, 3)
  }

  test("dropWhile") {
    listFoldable.dropWhile(_ < 4).viewAll shouldEqual List(4, 5, 6)
  }

  test("intercalate") {
    stringListFoldable.intercalate(", ") shouldEqual "Hello, World!"
    listFoldable.intercalate(1) shouldEqual 26
  }

  test("mkString") {
    stringListFoldable.mkString() shouldEqual "HelloWorld!"
    stringListFoldable.mkString(", ") shouldEqual "Hello, World!"
    stringListFoldable.mkString("[", " ", "]") shouldEqual "[Hello World!]"
  }

  test("monomorphic both") {
    ("Hello", "World!").bifold.viewAll shouldEqual List("Hello", "World!")
    ("Hello ", "World").bifold.foldRight("!")(_ ++ _) shouldEqual "Hello World!"
    ("Hello ", "World!").bifold.foldLeft("!")(_ ++ _) shouldEqual "!Hello World!"
  }

  test("polymorphic both") {
    ("1", "2").bifold_[Int].foldMap(parseInt) shouldEqual 3.some
    ("NaN ", "2").bifold_[Int].foldMap(parseInt) shouldEqual 2.some
  }

  test("filter using fold") {
    val fold =
      Getter[Whole](_.part) andThen
        Prism.fromPartial[Int, Int] { case i if i < 5 => i }(identity)
    val appliedFold = List(Whole(1), Whole(9), Whole(2)).foldable.filterF(fold)

    appliedFold.viewAll shouldEqual List(Whole(1), Whole(2))
  }

  test("filter using traversal") {
    val traversal =
      Lens[Whole, Int](_.part)(const(i => Whole(i))) andThen
        Prism.fromPartial[Int, Int] { case i if i < 5 => i }(identity)
    val appliedFold = List(Whole(1), Whole(9), Whole(2)).foldable.filterF(traversal)

    appliedFold.viewAll shouldEqual List(Whole(1), Whole(2))
  }
}
