package proptics.std

import cats.Applicative
import cats.syntax.traverse._

import proptics.rank2types.LensLike
import proptics.std.list._
import proptics.{Iso, Iso_, Traversal, Traversal_}

trait StringOptics {
  /** a monomorphic [[Iso]] from a string to a list of chars */
  final val stringToChars: Iso[String, List[Char]] = charsToString.reverse

  /** fold over the individual words of a String */
  val words: Traversal[String, String] = mkString(" ") compose split("\\s+")

  /** shows all elements of a collection in a string using a separator string */
  def mkString(sep: String): Iso_[String, String, String, List[String]] =
    Iso_.iso[String, String, String, List[String]](identity)(_.mkString(sep))

  /** splits this string around matches of the given regex */
  def split(regex: String): Traversal_[String, List[String], String, String] =
    Traversal_.wander(new LensLike[String, List[String], String, String] {
      override def apply[F[_]](f: String => F[String])(implicit ev: Applicative[F]): String => F[List[String]] =
        _.split(regex).toList.traverse(f)
    })
}
