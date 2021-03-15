package proptics.instances

import proptics.Lens
import proptics.typeclass.Field3

trait Field3Instances {
  final def third[A, B, C](implicit ev: Field3[(A, B, C), C]): Lens[(A, B, C), C] = ev.third

  implicit final def field3Tuple3[A, B, C]: Field3[(A, B, C), C] = new Field3[(A, B, C), C] {
    override def third: Lens[(A, B, C), C] =
      Lens[(A, B, C), C](_._3)(t => a => t.copy(_3 = a))
  }

  implicit final def field3Tuple4[A, B, C, D]: Field3[(A, B, C, D), C] = new Field3[(A, B, C, D), C] {
    override def third: Lens[(A, B, C, D), C] =
      Lens[(A, B, C, D), C](_._3)(t => a => t.copy(_3 = a))
  }

  implicit final def field3Tuple5[A, B, C, D, E]: Field3[(A, B, C, D, E), C] = new Field3[(A, B, C, D, E), C] {
    override def third: Lens[(A, B, C, D, E), C] =
      Lens[(A, B, C, D, E), C](_._3)(t => a => t.copy(_3 = a))
  }
}
