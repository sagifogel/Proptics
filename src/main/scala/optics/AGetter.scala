package optics
import cats.arrow.Arrow
import cats.syntax.arrow._
import optics.syntax.GetterSyntax._

/**
 * A [[AGetter]] is a [[Fold]] which has the same return type as the type of the target of the fold.
 *
 * @tparam S the source of an [[AGetter]]
 * @tparam T the modified source of an [[AGetter]]
 * @tparam A the target of an [[AGetter]]
 * @tparam B the modified target of an [[AGetter]]
 */
abstract class AGetter[S, T, A, B] extends Fold[A, S, T, A, B] { self =>
  def cloneGetter[R]: Getter[R, S, T, A, B] = Getter(self.view)

  def takeBoth[R, C, D](that: AGetter[S, T, C, D])(implicit ev: Arrow[* => *]): Getter[R, S, T, (A, C), (B, D)] =
    Getter(self.view _ &&& that.view)
}

object AGetter {
}