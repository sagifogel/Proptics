package proptics.instances

import scala.reflect.ClassTag

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}

import proptics.Iso
import proptics.typeclass.Reverse

trait ReverseInstances extends ScalaVersionSpecificReverseInstances {
  final def reverse[S, T](implicit ev: Reverse[S, T]): Iso[S, T] = ev.reverse

  implicit final val reverseString: Reverse[String, String] = new Reverse[String, String] {
    override def reverse: Iso[String, String] = Iso.involuted[String](_.reverse)
  }

  implicit def reverseTuple2[A, B]: Reverse[(A, B), (B, A)] = new Reverse[(A, B), (B, A)] {
    override def reverse: Iso[(A, B), (B, A)] = Iso.iso[(A, B), (B, A)](_.swap)(_.swap)
  }

  implicit def reverseTuple3[A, B, C]: Reverse[(A, B, C), (C, B, A)] = new Reverse[(A, B, C), (C, B, A)] {
    override def reverse: Iso[(A, B, C), (C, B, A)] =
      Iso.iso[(A, B, C), (C, B, A)](t => (t._3, t._2, t._1))(t => (t._3, t._2, t._1))
  }

  implicit def reverseTuple4[A, B, C, D]: Reverse[(A, B, C, D), (D, C, B, A)] = new Reverse[(A, B, C, D), (D, C, B, A)] {
    override def reverse: Iso[(A, B, C, D), (D, C, B, A)] =
      Iso.iso[(A, B, C, D), (D, C, B, A)](t => (t._4, t._3, t._2, t._1))(t => (t._4, t._3, t._2, t._1))
  }

  implicit def reverseTuple5[A, B, C, D, E]: Reverse[(A, B, C, D, E), (E, D, C, B, A)] = new Reverse[(A, B, C, D, E), (E, D, C, B, A)] {
    override def reverse: Iso[(A, B, C, D, E), (E, D, C, B, A)] =
      Iso.iso[(A, B, C, D, E), (E, D, C, B, A)](t => (t._5, t._4, t._3, t._2, t._1))(t => (t._5, t._4, t._3, t._2, t._1))
  }

  implicit final def reverseArray[A: ClassTag]: Reverse[Array[A], Array[A]] = new Reverse[Array[A], Array[A]] {
    override def reverse: Iso[Array[A], Array[A]] = Iso.involuted[Array[A]](_.reverse)
  }

  implicit final def reverseVector[A]: Reverse[Vector[A], Vector[A]] = new Reverse[Vector[A], Vector[A]] {
    override def reverse: Iso[Vector[A], Vector[A]] = Iso.involuted[Vector[A]](_.reverse)
  }

  implicit final def reverseList[A]: Reverse[List[A], List[A]] = new Reverse[List[A], List[A]] {
    override def reverse: Iso[List[A], List[A]] = Iso.involuted[List[A]](_.reverse)
  }

  implicit final def reverseChain[A]: Reverse[Chain[A], Chain[A]] = new Reverse[Chain[A], Chain[A]] {
    override def reverse: Iso[Chain[A], Chain[A]] = Iso.involuted[Chain[A]](_.reverse)
  }

  implicit final def reverseNonEmptyVector[A]: Reverse[NonEmptyVector[A], NonEmptyVector[A]] = new Reverse[NonEmptyVector[A], NonEmptyVector[A]] {
    override def reverse: Iso[NonEmptyVector[A], NonEmptyVector[A]] = Iso.involuted[NonEmptyVector[A]](_.reverse)
  }

  implicit final def reverseNonEmptyList[A]: Reverse[NonEmptyList[A], NonEmptyList[A]] = new Reverse[NonEmptyList[A], NonEmptyList[A]] {
    override def reverse: Iso[NonEmptyList[A], NonEmptyList[A]] = Iso.involuted[NonEmptyList[A]](_.reverse)
  }

  implicit final def reverseNonEmptyChain[A]: Reverse[NonEmptyChain[A], NonEmptyChain[A]] = new Reverse[NonEmptyChain[A], NonEmptyChain[A]] {
    override def reverse: Iso[NonEmptyChain[A], NonEmptyChain[A]] = Iso.involuted[NonEmptyChain[A]](_.reverse)
  }
}
