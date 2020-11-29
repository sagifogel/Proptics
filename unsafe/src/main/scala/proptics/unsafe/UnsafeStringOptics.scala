package proptics.unsafe

import cats.Applicative
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.traverse._
import proptics.Traversal
import proptics.profunctor.{Traversing, Wander}

trait UnsafeStringOptics {
  val words: Traversal[String, String] = new Traversal[String, String] {
    override private[proptics] def apply[P[_, _]](pab: P[String, String])(implicit ev0: Wander[P]): P[String, String] = {
      val traversing = new Traversing[String, String, String, String] {
        override def apply[F[_]](f: String => F[String])(s: String)(implicit ev1: Applicative[F]): F[String] =
          ev1.map(s.split("\\s+").toList.traverse(f))(_.intercalate(" "))
      }

      ev0.wander(traversing)(pab)
    }
  }
}
