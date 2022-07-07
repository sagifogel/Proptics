package proptics.specs

import scala.Function.const
import scala.util.Random

import cats.data.State
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.option._
import spire.std.boolean._
import spire.std.int._

import proptics._
import proptics.instances.each._
import proptics.specs.compose._
import proptics.syntax.applied.all._

class AppliedTraversalSpec extends PropticsSuite {
  val plusOne: Int => Int = _ + 1
  val someEven: Int => Option[Int] = i => if (i % 2 == 0) i.some else none[Int]
  val listTraversal: AppliedTraversal[List[Int], Int] = list.each
  val emptyTraversal: AppliedTraversal[List[Int], Int] = emptyList.each
  val positiveBoolTraversal: AppliedTraversal[List[Boolean], Boolean] = boolList.map(const(true)).each
  val negativeBoolTraversal: AppliedTraversal[List[Boolean], Boolean] = falseBoolList.each

  test("viewAll") {
    listTraversal.viewAll shouldEqual list
    emptyTraversal.viewAll shouldEqual emptyList
  }

  test("preview") {
    listTraversal.preview shouldEqual Some(1)
    emptyTraversal.preview shouldEqual None
  }

  test("set") {
    listTraversal.set(0) shouldEqual list.map(const(0))
  }

  test("over") {
    listTraversal.over(plusOne) shouldEqual list.map(plusOne)
  }

  test("traverse") {
    listTraversal.traverse(_.some) shouldEqual list.some
    listTraversal.traverse(someEven) shouldEqual None
    listTraversal.traverse(_.some) shouldEqual listTraversal.overF(_.some)
  }

  test("foldMap") {
    listTraversal.foldMap(_.toString) shouldEqual list.map(_.toString).intercalate("")
  }

  test("fold") {
    listTraversal.fold shouldEqual list.sum
    emptyTraversal.fold shouldEqual 0
    listTraversal.view shouldEqual listTraversal.fold
  }

  test("foldRight") {
    listTraversal.foldRight(emptyList)(_ :: _) shouldEqual list
  }

  test("foldLeft") {
    listTraversal.foldLeft(emptyList)((ls, a) => a :: ls) shouldEqual list.reverse
  }

  test("sequence_") {
    listTraversal.sequence_[Option] shouldEqual ().some
  }

  test("traverse_") {
    listTraversal.traverse_(_.some) shouldEqual Some(())
    listTraversal.traverse_(someEven) shouldEqual None
  }

  test("isEmpty") {
    listTraversal.isEmpty shouldEqual false
    emptyTraversal.isEmpty shouldEqual true
  }

  test("nonEmpty") {
    listTraversal.nonEmpty shouldEqual true
    emptyTraversal.nonEmpty shouldEqual false
    listTraversal.nonEmpty shouldEqual !listTraversal.isEmpty
  }

  test("length") {
    listTraversal.length shouldEqual list.length
    emptyTraversal.length shouldEqual 0
  }

  test("find") {
    listTraversal.find(greaterThan5) shouldEqual list.find(greaterThan5)
    listTraversal.find(greaterThan10) shouldEqual None
  }

  test("first") {
    listTraversal.first shouldEqual list.head.some
    emptyTraversal.first shouldEqual None
  }

  test("last") {
    listTraversal.last shouldEqual list.last.some
    emptyTraversal.last shouldEqual None
  }

  test("minimum") {
    Random.shuffle(list).each.minimum shouldEqual list.head.some
    emptyTraversal.minimum shouldEqual None
  }

  test("maximum") {
    Random.shuffle(list).each.maximum shouldEqual list.last.some
    emptyTraversal.maximum shouldEqual None
  }

  test("toArray") {
    listTraversal.toArray shouldEqual list.toArray
  }

  test("toList") {
    listTraversal.toList shouldEqual list
  }

  test("use") {
    implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](1)

    listTraversal.use.runA(list).value shouldEqual list
  }

  test("sum") {
    listTraversal.sum shouldEqual list.sum
  }

  test("product") {
    listTraversal.product shouldEqual list.product
    emptyTraversal.product shouldEqual 1
  }

  test("forall") {
    listTraversal.forall(_ < 10) shouldEqual true
    emptyTraversal.forall(_ < 10) shouldEqual true
    listTraversal.forall(_ > 10) shouldEqual false
    emptyTraversal.forall(_ > 10) shouldEqual true
  }

  test("forall using heyting") {
    listTraversal.forallH(_ < 10) shouldEqual true
    emptyTraversal.forallH(_ < 10) shouldEqual true
    listTraversal.forallH(_ > 10) shouldEqual false
    emptyTraversal.forallH(_ > 10) shouldEqual true
  }

  test("and") {
    boolList.traversal_[Boolean].and shouldEqual false
    positiveBoolTraversal.and shouldEqual true
    negativeBoolTraversal.and shouldEqual false
  }

  test("or") {
    positiveBoolTraversal.or shouldEqual true
    negativeBoolTraversal.or shouldEqual false
  }

  test("any") {
    listTraversal.any(greaterThan5) shouldEqual true
    listTraversal.any(greaterThan10) shouldEqual false
  }

  test("exist") {
    listTraversal.exists(greaterThan5) shouldEqual true
    listTraversal.exists(greaterThan10) shouldEqual false
  }

  test("notExists") {
    listTraversal.notExists(greaterThan5) shouldEqual false
    listTraversal.notExists(greaterThan10) shouldEqual true
    listTraversal.notExists(greaterThan10) shouldEqual !listTraversal.exists(greaterThan10)
  }

  test("contains") {
    listTraversal.contains(5) shouldEqual true
    listTraversal.contains(10) shouldEqual false
  }

  test("notContains") {
    listTraversal.notContains(5) shouldEqual false
    listTraversal.notContains(10) shouldEqual true
    listTraversal.notContains(10) shouldEqual !listTraversal.contains(10)
  }

  test("compose with Getter") {
    (traversal compose getter).fold(9) shouldEqual 9
  }

  test("compose with Fold") {
    (traversal compose fold).fold(9) shouldEqual 9
  }

  test("filterByIndex") {
    listTraversal.filterByIndex(_ < 3).viewAll shouldEqual list.take(3)
  }

  test("single") {
    listTraversal.single(1).viewAll shouldEqual List(2)
  }

  test("take") {
    val take3 = listTraversal.take(3)
    take3.viewAll shouldEqual List(1, 2, 3)
    take3.over(_ + 1) shouldEqual List(2, 3, 4, 4, 5, 6)
  }

  test("drop") {
    val drop3 = listTraversal.drop(3)
    drop3.viewAll shouldEqual List(4, 5, 6)
    drop3.over(_ + 1) shouldEqual List(1, 2, 3, 5, 6, 7)
  }

  test("takeWhile") {
    val take3 = listTraversal.takeWhile(_ < 4)
    take3.viewAll shouldEqual List(1, 2, 3)
    take3.over(_ + 1) shouldEqual List(2, 3, 4, 4, 5, 6)
  }

  test("dropWhile") {
    val drop3 = listTraversal.dropWhile(_ < 4)
    drop3.viewAll shouldEqual List(4, 5, 6)
    drop3.over(_ + 1) shouldEqual List(1, 2, 3, 5, 6, 7)
  }

  test("both") {
    val both = ("Hello", "World!").bitraverse_[Int]
    val both2 = ("Hello ", "World").bitraverse_[Int]

    both.viewAll shouldEqual List("Hello", "World!")
    both.over(_.length) shouldEqual ((5, 6))
    both2.foldRight("!")(_ ++ _) shouldEqual "Hello World!"
    both2.foldLeft("!")(_ ++ _) shouldEqual "!Hello World"
  }
}
