package proptics

import cats.Eq
import cats.syntax.eq._
import cats.laws._
import org.scalacheck.Prop
import org.scalacheck.Prop.{falsified, proved}
import org.scalacheck.util.Pretty

package object law {
  implicit def isEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    if (isEq.lhs === isEq.rhs) proved
    else
      falsified :| {
        val left = Pretty.pretty[A](isEq.rhs, Pretty.Params(0))
        val right = Pretty.pretty[A](isEq.lhs, Pretty.Params(0))
        s"Expected $right, but get $left"
      }
}
