package proptics.instances

import proptics.Lens
import proptics.typeclass.Field4

trait Field4Instances {
  final def fourth[A, B, C, D](implicit ev: Field4[(A, B, C, D), D]): Lens[(A, B, C, D), D] = ev.fourth

  implicit final def field4Tuple4[A, B, C, D]: Field4[(A, B, C, D), D] = new Field4[(A, B, C, D), D] {
    override def fourth: Lens[(A, B, C, D), D] =
      Lens[(A, B, C, D), D](_._4)(t => a => t.copy(_4 = a))
  }

  implicit final def field4Tuple5[A, B, C, D, E]: Field4[(A, B, C, D, E), D] = new Field4[(A, B, C, D, E), D] {
    override def fourth: Lens[(A, B, C, D, E), D] =
      Lens[(A, B, C, D, E), D](_._4)(t => a => t.copy(_4 = a))
  }
}
