package proptics
import cats.arrow.Arrow
import cats.syntax.arrow._
import proptics.internal.Forget
import proptics.syntax.GetterSyntax._

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
  private[AGetter] def apply[S, T, A, B](f: Forget[A, A, B] => Forget[A, S, T]): AGetter[S, T, A, B] = new AGetter[S, T, A, B] {
    override def apply(pab: Forget[A, A, B]): Forget[A, S, T] = f(pab)
  }

  def apply[R, S, T, A, B](f: S => A)(implicit ev: DummyImplicit): AGetter[S, T, A, B] = {
    AGetter((forget: Forget[A, A, B]) => Forget[A, S, T](forget.runForget compose f))
  }
}