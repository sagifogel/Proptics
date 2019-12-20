package optics

import cats.Id
import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._
import optics.internal.{Indexed, Shop}
import optics.profunctor.Star

import scala.Function.const

/**
 * Given a type whose "focus element" always exists,
 * a [[Lens]] provides a convenient way to view, set, and transform
 * that element.
 *
 * @tparam P an evidence of [[Strong]] [[cats.arrow.Profunctor]]
 * @tparam S the source of a [[Lens]]
 * @tparam T the modified source of a [[Lens]]
 * @tparam A the target of a [[Lens]]
 * @tparam B the modified target of a [[Lens]]
 */
abstract class Lens[P[_, _] : Strong, S, T, A, B] extends Optic[P, S, T, A, B] {
}

object Lens {
  def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T])(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    new Lens[P, S, T, A, B] {
      override def apply(pab: P[A, B]): P[S, T] = f(pab)
    }

  /**
   * Create a [[Lens]] from a getter/setter pair.
   */
  def lens[P[_, _], S, T, A, B](get: S => A)(set: S => B => T)(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    lens_((get, set).mapN(Tuple2.apply))

  def lens_[P[_, _], S, T, A, B](to: S => (A, B => T))(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    Lens(pab => {
      val first = ev.first[A, B, B => T](pab)
      ev.dimap(first)(to) { case (b, f) => f(b) }
    })

  def withLens[S, T, A, B, R](aLens: ALens[S, T, A, B])(f: (S => A) => (S => B => T) => R): R = {
    val shop = aLens(Shop(identity, const(identity)))

    f(shop.f)(shop.g)
  }

  def cloneLens[P[_, _], S, T, A, B](aLens: ALens[S, T, A, B])(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    withLens(aLens)(Lens.lens[P, S, T, A, B])

  def ilens_[P[_, _], I, S, T, A, B](to: S => ((I, A), B => T))(implicit ev: Strong[P]): IndexedLens[P, I, S, T, A, B] =
    IndexedLens[P, I, S, T, A, B](piab => {
      ev.dimap(ev.first[(I, A), B, B => T](piab))(to) { case (b, b2t) => b2t(b) }
    })

  def ilens[P[_, _], I, S, T, A, B](get: S => (I, A))(set: S => B => T)
                                   (implicit ev: Strong[P]): IndexedLens[P, I, S, T, A, B] = {
    ilens_((get, set).mapN(Tuple2.apply))
  }

  def withIndexedLens[P[_, _], I, S, T, A, B, R](anIndexedLens: AnIndexedLens[I, S, T, A, B])
                                                (f: (S => (I, A)) => (S => B => T) => R): R = {
    val shop = anIndexedLens(Indexed(Shop(identity, const(identity))))

    f(shop.f)(shop.g)
  }

  def cloneIndexedLens[P[_, _], I, S, T, A, B](anIndexedLens: AnIndexedLens[I, S, T, A, B])
                                              (implicit ev: Strong[P]): IndexedLens[P, I, S, T, A, B] = {
    withIndexedLens(anIndexedLens)(ilens[P, I, S, T, A, B])
  }

  /**
   * Converts a [[Lens]] into the form that [[Lens_]] accepts.
   *
   * Can be useful when defining a lens where the focus appears under multiple
   * constructors of an algebraic data type. This function would be called for
   * each case of the data type.
   */
  def lensStore[S, T, A, B](aLens: ALens[S, T, A, B])(s: S): (A, B => T) = {
     withLens(aLens)(sa => sbt => (sa, sbt).mapN(Tuple2.apply))(s)
  }
}

object Lens_ {
  def apply[P[_, _], S, A](f: P[A, A] => P[S, S])(implicit ev: Strong[P]): Lens_[P, S, A] = Lens(f)
}

object StarLens {

  import Lens.lens

  def apply[S, A](get: S => A)(set: S => A => S)(implicit ev: Strong[Star[Id, *, *]]): Lens_[Star[Id, *, *], S, A] =
    lens[Star[Id, *, *], S, S, A, A](get)(set)
}
