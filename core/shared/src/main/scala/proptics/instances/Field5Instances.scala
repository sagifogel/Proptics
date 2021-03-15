package proptics.instances

import proptics.Lens
import proptics.typeclass.Field5

trait Field5Instances {
  final def fifth[A, B, C, D, E](implicit ev: Field5[(A, B, C, D, E), E]): Lens[(A, B, C, D, E), E] = ev.fifth

  implicit final def field5Tuple5[A, B, C, D, E]: Field5[(A, B, C, D, E), E] = new Field5[(A, B, C, D, E), E] {
    override def fifth: Lens[(A, B, C, D, E), E] =
      Lens[(A, B, C, D, E), E](_._5)(t => a => t.copy(_5 = a))
  }
}
