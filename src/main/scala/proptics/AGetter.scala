package proptics
import cats.arrow.Arrow
import cats.syntax.arrow._
import proptics.internal.Forget

/**
 * A [[AGetter]] is a [[Fold]] which has the same return type as the type of the target of the fold.
 *
 * @tparam S the source of an [[AGetter]]
 * @tparam T the modified source of an [[AGetter]]
 * @tparam A the target of an [[AGetter]]
 * @tparam B the modified target of an [[AGetter]]
 */
abstract class AGetter[S, T, A, B] extends Fold[A, S, T, A, B] { self =>
  def cloneGetter[R]: Getter[S, T, A, B] = Getter(self.view)

  def takeBoth[R, C, D](that: AGetter[S, T, C, D])(implicit ev: Arrow[* => *]): Getter[S, T, (A, C), (B, D)] =
    Getter(self.view _ &&& that.view)

  def foldOf(s: S): A = self.foldMapOf(identity)(s)

  def view(s: S): A = self(Forget(identity[A])).runForget(s)
}

object AGetter {
  private[AGetter] def apply[S, T, A, B](f: Forget[A, A, B] => Forget[A, S, T]): AGetter[S, T, A, B] = new AGetter[S, T, A, B] {
    override def apply(forget: Forget[A, A, B]): Forget[A, S, T] = f(forget)
  }

  def apply[R, S, T, A, B](f: S => A)(implicit ev: DummyImplicit): AGetter[S, T, A, B] =
    AGetter((forget: Forget[A, A, B]) => Forget[A, S, T](forget.runForget compose f))
}

object AGetter_ {
  def apply[S, A](f: S => A): AGetter_[S, A] = AGetter(f)
}