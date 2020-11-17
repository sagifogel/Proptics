package proptics.instances

import scala.Function.const

import proptics.Lens

trait UnitInstances {
  final def unit[A]: Lens[A, Unit] = Lens[A, Unit](const(()))(const)
}
