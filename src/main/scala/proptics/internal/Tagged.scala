package optics.internal

import cats.arrow.Profunctor
import cats.{Applicative, Eq, Eval, Foldable, Functor, Order, Traverse}
import cats.syntax.order._
import cats.syntax.either._
import optics.profunctor.{Choice, Closed, Costrong}

import scala.Function.const

final case class Tagged[A, B](runTag: B)

abstract class TaggedInstances {
  implicit final def eqTagged[A, B](implicit ev: Eq[B]): Eq[Tagged[A, B]] = new Eq[Tagged[A, B]] {
    override def eqv(x: Tagged[A, B], y: Tagged[A, B]): Boolean = x.runTag === y.runTag
  }

  implicit final def orderTagged[A, B](implicit ev: Order[B]): Order[Tagged[A, B]] = new Order[Tagged[A, B]] {
    override def compare(x: Tagged[A, B], y: Tagged[A, B]): Int = x.runTag compare y.runTag
  }

  implicit final def functorTagged[F[_], C]: Functor[Tagged[C, *]]  = new Functor[Tagged[C, *]] {
    override def map[A, B](fa: Tagged[C, A])(f: A => B): Tagged[C, B] = Tagged(f(fa.runTag))
  }

  implicit final val profunctorTagged: Profunctor[Tagged]  = new Profunctor[Tagged] {
    override def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A)(g: B => D): Tagged[C, D] =
      Tagged(g(fab.runTag))
  }

  implicit final val choiceTagged: Choice[Tagged]  = new Choice[Tagged] {
    override def left[A, B, C](pab: Tagged[A, B]): Tagged[Either[A, C], Either[B, C]] = Tagged(pab.runTag.asLeft[C])

    override def right[A, B, C](pab: Tagged[B, C]): Tagged[Either[A, B], Either[A, C]] = Tagged(pab.runTag.asRight[A])

    override def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A)(g: B => D): Tagged[C, D] =
      profunctorTagged.dimap(fab)(f)(g)
  }

  implicit final val costrongTagged: Costrong[Tagged]  = new Costrong[Tagged] {
    override def unfirst[A, B, C](p: Tagged[(A, C), (B, C)]): Tagged[A, B] = Tagged(p.runTag._1)

    override def unsecond[A, B, C](p: Tagged[(A, B), (A, C)]): Tagged[B, C] = Tagged(p.runTag._2)

    override def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A)(g: B => D): Tagged[C, D] =
      profunctorTagged.dimap(fab)(f)(g)
  }

  implicit final val closedTagged: Closed[Tagged]  = new Closed[Tagged] {
    override def closed[A, B, C](pab: Tagged[A, B]): Tagged[C => A, C => B] =
      Tagged(const(pab.runTag))

    override def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A)(g: B => D): Tagged[C, D] =
      profunctorTagged.dimap(fab)(f)(g)
  }

  implicit final def foldableTagged[C]: Foldable[Tagged[C, *]]  = new Foldable[Tagged[C, *]] {
    override def foldLeft[A, B](fa: Tagged[C, A], b: B)(f: (B, A) => B): B = f(b, fa.runTag)

    override def foldRight[A, B](fa: Tagged[C, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa.runTag, lb)
  }

  implicit final def traverseTagged[C]: Traverse[Tagged[C, *]]  = new Traverse[Tagged[C, *]] {
    override def traverse[G[_], A, B](fa: Tagged[C, A])(f: A => G[B])(implicit ev: Applicative[G]): G[Tagged[C, B]] =
      ev.map(f(fa.runTag))(Tagged[C, B])

    override def foldLeft[A, B](fa: Tagged[C, A], b: B)(f: (B, A) => B): B =
      foldableTagged.foldLeft(fa, b)(f)

    override def foldRight[A, B](fa: Tagged[C, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldableTagged.foldRight(fa, lb)(f)
  }
}

object Tagged extends TaggedInstances