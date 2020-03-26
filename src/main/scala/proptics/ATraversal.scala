package proptics

import cats.Applicative
import proptics.internal.{Bazaar, RunBazaar}

/**
 * A [[Traversal]] with fixed type [[Bazaar]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[ATraversal]]
 * @tparam T the modified source of a [[ATraversal]]
 * @tparam A the target of a [[ATraversal]]
 * @tparam B the modified target of a [[ATraversal]]
 */
abstract class ATraversal[S, T, A, B] {
  private[proptics] def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T]
}

object ATraversal {
  private[proptics] def apply[S, T, A, B](f: Bazaar[* => *, A, B, A, B] => Bazaar[* => *, A, B, S, T]): ATraversal[S, T, A, B] = new ATraversal[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = f(bazaar)
  }

  def apply[S, T, A, B](get: S => A)(set: B => S => T): ATraversal[S, T, A, B] = new ATraversal[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] =
          ev.map(pafb(get(s)))(set(_)(s))
      }
    }
  }

  def apply[S, T, A, B](to: S => (A, B => T))(implicit ev: DummyImplicit): ATraversal[S, T, A, B] = new ATraversal[S, T, A, B] {
    override def apply(bazaar: Bazaar[* => *, A, B, A, B]): Bazaar[* => *, A, B, S, T] = new Bazaar[* => *, A, B, S, T] {
      override def runBazaar: RunBazaar[* => *, A, B, S, T] = new RunBazaar[* => *, A, B, S, T] {
        override def apply[F[_]](pafb: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = {
          val (a, b2t) = to(s)

          ev.map(pafb(a))(b2t)
        }
      }
    }
  }
}

object ATraversal_ {
  def apply[S, A](get: S => A)(set: A => S => S): ATraversal_[S, A] = ATraversal(get)(set)

  def apply[S, A](to: S => (A, A => S)): ATraversal_[S, A] = ATraversal(to)
}
