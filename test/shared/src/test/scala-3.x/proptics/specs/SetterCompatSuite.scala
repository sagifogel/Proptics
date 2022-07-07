package proptics.specs

import spire.algebra.Field

trait SetterCompatSuite extends PropticsSuite {
  implicit val intField: Field[Int] = new Field[Int] {
    override def negate(x: Int): Int = Math.negateExact(x)

    override def zero: Int = 0

    override def one: Int = 1

    override def times(x: Int, y: Int): Int = x * y

    override def plus(x: Int, y: Int): Int = x + y

    override def div(x: Int, y: Int): Int = x / y
  }
}
