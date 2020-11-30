package proptics.std

import scala.Function.const

import proptics.Lens

trait UnitOptics {
  final def unit[A]: Lens[A, Unit] = Lens[A, Unit](const(()))(const)
}
