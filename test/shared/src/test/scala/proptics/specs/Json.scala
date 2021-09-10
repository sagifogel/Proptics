package proptics.specs

import cats.Eq
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}

sealed trait Json
case class JNull() extends Json
case class JString(value: String) extends Json
case class JNumber(value: Double) extends Json

object Json {
  implicit val eqJson: Eq[Json] = new Eq[Json] {
    override def eqv(x: Json, y: Json): Boolean = (x, y) match {
      case (JNull(), JNull()) => true
      case (JString(v1), JString(v2)) => v1 === v2
      case (JNumber(v1), JNumber(v2)) => v1 === v2
      case _ => false
    }
  }

  implicit val eqJString: Eq[JString] = new Eq[JString] {
    override def eqv(x: JString, y: JString): Boolean =
      x.value === y.value
  }

  implicit val jNullArb: Arbitrary[JNull] = Arbitrary(Gen.const(JNull()))
  implicit val jStringArb: Arbitrary[JString] = Arbitrary(arbitrary[String].map(JString.apply))
  implicit val jNumberArb: Arbitrary[JNumber] = Arbitrary(arbitrary[Double].map(JNumber.apply))
  implicit val arbitraryJson: Arbitrary[Json] =
    Arbitrary(
      Gen.oneOf(
        jNullArb.arbitrary,
        jStringArb.arbitrary,
        jNumberArb.arbitrary
      ))

  implicit val cogenJString: Cogen[JString] = Cogen.cogenString.contramap[JString](_.value)
}
