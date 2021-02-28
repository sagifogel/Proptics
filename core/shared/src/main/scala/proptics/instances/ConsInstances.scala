package proptics.instances

import cats.data.Chain
import proptics.{AffineTraversal, Cons, Prism}

import scala.reflect.ClassTag

trait ConsInstances {
  final def cons[S, A](implicit ev: Cons[S, A]): Prism[S, (A, S)] = ev.cons

  final def headOption[S, A](implicit ev: Cons[S, A]): AffineTraversal[S, A] = ev.headOption

  final def tailOption[S, A](implicit ev: Cons[S, A]): AffineTraversal[S, S] = ev.tailOption

  implicit final def consString: Cons[String, Char] = new Cons[String, Char] {
    override def cons: Prism[String, (Char, String)] =
      Prism.fromPreview[String, (Char, String)](str => str.headOption.map((_, str.tail))) {
        case (head, tail) => s"$head$tail"
      }
  }

  implicit final def consArray[A : ClassTag]: Cons[Array[A], A] = new Cons[Array[A], A] {
    override def cons: Prism[Array[A], (A, Array[A])] =
      Prism.fromPreview[Array[A], (A, Array[A])](arr => arr.headOption.map((_, arr.tail))) {
        case (x, xs) => x +: xs
      }
  }

  implicit final def consList[A]: Cons[List[A], A] = new Cons[List[A], A] {
    override def cons: Prism[List[A], (A, List[A])] =
      Prism.fromPartial[List[A], (A, List[A])] { case x :: xs => (x, xs) } { case (x, xs) => x :: xs }
  }

  implicit final def consVector[A]: Cons[Vector[A], A] = new Cons[Vector[A], A] {
    override def cons: Prism[Vector[A], (A, Vector[A])] =
      Prism.fromPreview[Vector[A], (A, Vector[A])](vector => vector.headOption.map((_, vector.tail))) {
        case (x, xs) => x +: xs
      }
  }

  implicit final def consChain[A]: Cons[Chain[A], A] = new Cons[Chain[A], A] {
    override def cons: Prism[Chain[A], (A, Chain[A])] =
      Prism.fromPreview[Chain[A], (A, Chain[A])](_.uncons){ case (x, xs) => x +: xs }
  }
}
