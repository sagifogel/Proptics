package proptics.instances

import proptics.Lens
import proptics.typeclass.Field1

trait Field1Instances {
  final def first[A, B](implicit ev: Field1[(A, B), A]): Lens[(A, B), A] = ev.first

  implicit final def field1Tuple2[A, B]: Field1[(A, B), A] = new Field1[(A, B), A] {
    override def first: Lens[(A, B), A] =
      Lens[(A, B), A](_._1)(t => a => t.copy(_1 = a))
  }

  implicit final def field1Tuple3[A, B, C]: Field1[(A, B, C), A] = new Field1[(A, B, C), A] {
    override def first: Lens[(A, B, C), A] =
      Lens[(A, B, C), A](_._1)(t => a => t.copy(_1 = a))
  }

  implicit final def field1Tuple4[A, B, C, D]: Field1[(A, B, C, D), A] = new Field1[(A, B, C, D), A] {
    override def first: Lens[(A, B, C, D), A] =
      Lens[(A, B, C, D), A](_._1)(t => a => t.copy(_1 = a))
  }

  implicit final def field1Tuple5[A, B, C, D, E]: Field1[(A, B, C, D, E), A] = new Field1[(A, B, C, D, E), A] {
    override def first: Lens[(A, B, C, D, E), A] =
      Lens[(A, B, C, D, E), A](_._1)(t => a => t.copy(_1 = a))
  }
}
