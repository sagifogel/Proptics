package proptics.law

import cats.Eq
import cats.instances.int._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import proptics.Iso

class IsoSpec extends PropticsSuite {
  case class Wrapper[T](i: T)
  implicit val intWrapperEq: Eq[Wrapper[Int]] = Eq.fromUniversalEquals[Wrapper[Int]]
  implicit val intWrapperArbitrary: Arbitrary[Wrapper[Int]] = Arbitrary[Wrapper[Int]](Arbitrary.arbInt.arbitrary.map(Wrapper[Int]))
  val iso: Iso[Wrapper[Int], Int] = Iso.iso[Wrapper[Int], Int](_.i)(Wrapper[Int])

  checkAll("apply Iso", IsoRules(iso))

  test("view") {
    iso.view(Wrapper[Int](5)) shouldEqual 5
  }
}
