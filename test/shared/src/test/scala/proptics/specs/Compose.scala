package proptics.specs

import proptics.{ALens, APrism, ATraversal, AffineTraversal, AnAffineTraversal, Fold, Getter, Grate, Iso, Lens, Prism, Review, Setter, Traversal}
import cats.syntax.either._

import Function.const

object Compose {
  val iso: Iso[Int, Int] = Iso[Int, Int](identity[Int] _)(identity)
  val lens: Lens[Int, Int] = Lens[Int, Int](identity)(const(identity))
  val aLens: ALens[Int, Int] = ALens[Int, Int](identity)(const(identity))
  val prism: Prism[Int, Int] = Prism[Int, Int](_.asRight[Int])(identity)
  val aPrism: APrism[Int, Int] = APrism[Int, Int](_.asRight[Int])(identity)
  val affineTraversal: AffineTraversal[Int, Int] = AffineTraversal[Int, Int](_.asRight[Int])(const(identity))
  val anAffineTraversal: AnAffineTraversal[Int, Int] = AnAffineTraversal[Int, Int](_.asRight[Int])(const(identity))
  val traversal: Traversal[Int, Int] = Traversal[Int, Int](identity)(const(identity))
  val aTraversal: ATraversal[Int, Int] = ATraversal[Int, Int](identity)(const(identity))
  val setter: Setter[Int, Int] = Setter[Int, Int](f => f)
  val getter: Getter[Int, Int] = Getter[Int, Int](identity)
  val fold: Fold[Int, Int] = Fold[Int, Int](identity)
  val grate: Grate[Int, Int] = Grate[Int, Int](_(identity))
  val review: Review[Int, Int] = Review[Int, Int](identity)
}
