package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.std.option.some
import proptics.{AffineTraversal, Lens}

/** The [[At]] typeclass provides a [[Lens]] that can be used to read, write or delete the value associated with a key in a Map-like container
  *
  * @tparam S the source of a [[Lens]]
  * @tparam I the index of a [[Lens]]
  * @tparam A an optional focus of a [[Lens]]
  */
@implicitNotFound("Could not find an instance of At[${S}, ${I}, ${A}]")
trait At[S, I, A] extends Index[S, I, A] {
  def at(i: I): Lens[S, Option[A]]
}

object At {
  /** summon an instance of [[At]] */
  @inline def apply[S, I, A](implicit ev: At[S, I, A]): At[S, I, A] = ev

  /** create an [[At]], using an index to a view function and a setter function */
  final def at[S, I, A](view: I => S => Option[A])(set: I => S => Option[A] => S): At[S, I, A] = new At[S, I, A] {
    override def at(i: I): Lens[S, Option[A]] = Lens[S, Option[A]](view(i))(set(i))

    override def ix(i: I): AffineTraversal[S, A] =
      at(i) andThen (some[A] andThen AffineTraversal.id[A])
  }
}
