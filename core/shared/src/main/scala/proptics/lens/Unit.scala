package proptics.lens

import proptics.Lens

import scala.Function.const

object Unit {
  def unit[A]: Lens[A, Unit] = Lens[A, Unit](const(()))(const)
}
