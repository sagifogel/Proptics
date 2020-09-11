package proptics.specs

import cats.Eq
import cats.data.State
import org.scalacheck.Arbitrary

final case class Whole(part: Int)

object Whole {
  implicit val state: State[Whole, Int] = State.pure[Whole, Int](9)
  implicit val eqWhole: Eq[Whole] = Eq.fromUniversalEquals[Whole]
  implicit val arbitraryWhole: Arbitrary[Whole] = Arbitrary[Whole](Arbitrary.arbInt.arbitrary.map(Whole.apply))
}
