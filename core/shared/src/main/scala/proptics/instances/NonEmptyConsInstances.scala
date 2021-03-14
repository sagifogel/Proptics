package proptics.instances

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector, OneAnd}

import proptics.{Iso, Lens, NonEmptyCons}

trait NonEmptyConsInstances {
  final def nonEmptyCons[S, H, T](implicit ev: NonEmptyCons[S, H, T]): Iso[S, (H, T)] = ev.nonEmptyCons

  final def head[S, H, T](implicit ev: NonEmptyCons[S, H, T]): Lens[S, H] = ev.head

  final def tail[S, H, T](implicit ev: NonEmptyCons[S, H, T]): Lens[S, T] = ev.tail

  implicit def nonEmptyConsTuple2[A, B]: NonEmptyCons[(A, B), A, B] = new NonEmptyCons[(A, B), A, B] {
    override def nonEmptyCons: Iso[(A, B), (A, B)] = Iso.iso[(A, B), (A, B)](identity)(identity)
  }

  implicit final def nonEmptyConsTuple3[A, B, C]: NonEmptyCons[(A, B, C), A, (B, C)] = new NonEmptyCons[(A, B, C), A, (B, C)] {
    override def nonEmptyCons: Iso[(A, B, C), (A, (B, C))] =
      Iso.iso[(A, B, C), (A, (B, C))](t => (t._1, (t._2, t._3))) { case (h, t) => (h, t._1, t._2) }
  }

  implicit final def nonEmptyConsTuple4[A, B, C, D]: NonEmptyCons[(A, B, C, D), A, (B, C, D)] = new NonEmptyCons[(A, B, C, D), A, (B, C, D)] {
    override def nonEmptyCons: Iso[(A, B, C, D), (A, (B, C, D))] =
      Iso.iso[(A, B, C, D), (A, (B, C, D))](t => (t._1, (t._2, t._3, t._4))) { case (h, t) => (h, t._1, t._2, t._3) }
  }

  implicit final def nonEmptyConsTuple5[A, B, C, D, E]: NonEmptyCons[(A, B, C, D, E), A, (B, C, D, E)] = new NonEmptyCons[(A, B, C, D, E), A, (B, C, D, E)] {
    override def nonEmptyCons: Iso[(A, B, C, D, E), (A, (B, C, D, E))] =
      Iso.iso[(A, B, C, D, E), (A, (B, C, D, E))](t => (t._1, (t._2, t._3, t._4, t._5))) { case (h, t) => (h, t._1, t._2, t._3, t._4) }
  }

  implicit final def nonEmptyConsNonEmptyVector[A]: NonEmptyCons[NonEmptyVector[A], A, Vector[A]] = new NonEmptyCons[NonEmptyVector[A], A, Vector[A]] {
    override def nonEmptyCons: Iso[NonEmptyVector[A], (A, Vector[A])] =
      Iso.iso[NonEmptyVector[A], (A, Vector[A])](nev => (nev.head, nev.tail))(NonEmptyVector.apply[A] _ tupled _)
  }

  implicit final def nonEmptyConsNoneEmptyList[A]: NonEmptyCons[NonEmptyList[A], A, List[A]] = new NonEmptyCons[NonEmptyList[A], A, List[A]] {
    override def nonEmptyCons: Iso[NonEmptyList[A], (A, List[A])] =
      Iso.iso[NonEmptyList[A], (A, List[A])](nel => (nel.head, nel.tail))(NonEmptyList.apply[A] _ tupled _)
  }

  implicit final def nonEmptyConsNonEmptyChain[A]: NonEmptyCons[NonEmptyChain[A], A, Chain[A]] = new NonEmptyCons[NonEmptyChain[A], A, Chain[A]] {
    override def nonEmptyCons: Iso[NonEmptyChain[A], (A, Chain[A])] =
      Iso.iso[NonEmptyChain[A], (A, Chain[A])](_.uncons) { case (head, tail) => NonEmptyChain(head, tail.toList: _*) }
  }

  implicit final def nonEmptyConsOneAnd[F[_], A]: NonEmptyCons[OneAnd[F, A], A, F[A]] = new NonEmptyCons[OneAnd[F, A], A, F[A]] {
    override def nonEmptyCons: Iso[OneAnd[F, A], (A, F[A])] =
      Iso.iso[OneAnd[F, A], (A, F[A])](nel => (nel.head, nel.tail))(OneAnd.apply[F, A] _ tupled _)
  }
}
