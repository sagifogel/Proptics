package proptics.instances

import proptics.Lens
import proptics.typeclass.Field2

trait Field2Instances {
  final def second[A, B](implicit ev: Field2[(A, B), B]): Lens[(A, B), B] = ev.second

  implicit final def field2Tuple2[A, B]: Field2[(A, B), B] = new Field2[(A, B), B] {
    override def second: Lens[(A, B), B] =
      Lens[(A, B), B](_._2)(t => a => t.copy(_2 = a))
  }

  implicit final def field2Tuple3[A, B, C]: Field2[(A, B, C), B] = new Field2[(A, B, C), B] {
    override def second: Lens[(A, B, C), B] =
      Lens[(A, B, C), B](_._2)(t => a => t.copy(_2 = a))
  }

  implicit final def field2Tuple4[A, B, C, D]: Field2[(A, B, C, D), B] = new Field2[(A, B, C, D), B] {
    override def second: Lens[(A, B, C, D), B] =
      Lens[(A, B, C, D), B](_._2)(t => a => t.copy(_2 = a))
  }

  implicit final def field2Tuple5[A, B, C, D, E]: Field2[(A, B, C, D, E), B] = new Field2[(A, B, C, D, E), B] {
    override def second: Lens[(A, B, C, D, E), B] =
      Lens[(A, B, C, D, E), B](_._2)(t => a => t.copy(_2 = a))
  }
}
