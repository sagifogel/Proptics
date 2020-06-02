package proptics

import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._
import proptics.profunctor.Choice
import proptics.rank2types.Rank2TypeAffineTraversalLike

/**
  * An affine traversal has at most one focus, but is not a [[Prism_]]
  *
  * @tparam S the source of an [[AffineTraversal_]]
  * @tparam T the modified source of an [[AffineTraversal_]]
  * @tparam A the focus of an [[AffineTraversal_]]
  * @tparam B the modified focus of an [[AffineTraversal_]]
  */
abstract class AffineTraversal_[S, T, A, B] extends Serializable { self =>
  def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T]
}

object AffineTraversal_ {

  /** create a polymorphic [[AffineTraversal_]] from Rank2TypeAffineTraversalLike encoding */
  def apply[S, T, A, B](f: Rank2TypeAffineTraversalLike[S, T, A, B]): AffineTraversal_[S, T, A, B] = new AffineTraversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = f(pab)
  }

  /** create a polyomorphic [[AffineTraversal_]] from a getter/setter pair */
  def apply[S, T, A, B](get: S => Either[T, A])(_set: S => B => T): AffineTraversal_[S, T, A, B] =
    AffineTraversal_((get, _set).mapN(Tuple2.apply))

  /** create a polymorphic [[AffineTraversal_]] from a combined getter/setter */
  def apply[S, T, A, B](to: S => (Either[T, A], B => T)): AffineTraversal_[S, T, A, B] = new AffineTraversal_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T] = {
      val eitherPab = ev1.first[Either[T, A], Either[T, B], B => T](ev0.right(pab))

      ev0.dimap(eitherPab)(to) { case (f, b) => f.fold(identity, b) }
    }
  }
}

object AffineTraversal {

  /** create a momnomorphic [[AffineTraversal]] from a getter/setter pair */
  def apply[S, A](get: S => Either[S, A])(set: S => A => S): AffineTraversal[S, A] = AffineTraversal_(get)(set)

  /** create a monomorphic [[AffineTraversal]] from a combined getter/setter */
  def apply[S, A](to: S => (Either[S, A], A => S)): AffineTraversal[S, A] = AffineTraversal_(to)
}
