package proptics.instances

import proptics.Lens

import scala.Function.const

trait UnitInstances {
  final def unit[A]: Lens[A, Unit] = Lens[A, Unit](const(()))(const)
}
