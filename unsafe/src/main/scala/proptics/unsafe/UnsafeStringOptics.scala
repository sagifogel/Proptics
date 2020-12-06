package proptics.unsafe

import cats.Applicative
import cats.instances.list._
import cats.syntax.traverse._
import proptics.rank2types.LensLike
import proptics.{Iso_, Traversal, Traversal_}

trait UnsafeStringOptics {
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
