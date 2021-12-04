package proptics.syntax.applied

import proptics.applied.AppliedLens
import proptics.typeclass._
import proptics.{AppliedLens, Lens}

trait AppliedFieldsSyntax {
  implicit final def tuple2ToAppliedLensOps[A, B](s: (A, B)): Tuple2ToAppliedLensOps[A, B] = Tuple2ToAppliedLensOps(s)

  implicit final def appliedLensTuple2Ops[A, B, C](appliedLens: AppliedLens[(A, B), C]): AppliedLensTuple2Ops[A, B, C] = AppliedLensTuple2Ops(appliedLens)

  implicit final def tuple3ToAppliedLensOps[A, B, C](s: (A, B, C)): Tuple3ToAppliedLensOps[A, B, C] = Tuple3ToAppliedLensOps(s)

  implicit final def appliedLensTuple3Ops[A, B, C, D](s: AppliedLens[(A, B, C), D]): AppliedFieldsTuple3Ops[A, B, C, D] = AppliedFieldsTuple3Ops(s)

  implicit final def tuple4ToAppliedLensOps[A, B, C, D](s: (A, B, C, D)): Tuple4ToAppliedLensOps[A, B, C, D] = Tuple4ToAppliedLensOps(s)

  implicit final def appliedLensTuple4Ops[A, B, C, D, E](s: AppliedLens[(A, B, C, D), E]): AppliedFieldsTuple4Ops[A, B, C, D, E] = AppliedFieldsTuple4Ops(s)

  implicit final def tuple5ToAppliedLensOps[A, B, C, D, E](s: (A, B, C, D, E)): Tuple5ToAppliedLensOps[A, B, C, D, E] = Tuple5ToAppliedLensOps(s)

  implicit final def appliedLensTuple5Ops[A, B, C, D, E, F](s: AppliedLens[(A, B, C, D, E), F]): AppliedFieldsTuple5Ops[A, B, C, D, E, F] = AppliedFieldsTuple5Ops(s)
}

case class Tuple2ToAppliedLensOps[A, B](private val s: (A, B)) extends AnyVal {
  def first[D](implicit ev: Field1[(A, B), A]): AppliedLens[(A, B), A] = AppliedLens(s, ev.first)

  def second[D](implicit ev: Field2[(A, B), B]): AppliedLens[(A, B), B] = AppliedLens(s, ev.second)
}

case class AppliedLensTuple2Ops[A, B, C](private val appliedLens: AppliedLens[(A, B), C]) extends AnyVal {
  private def value: (A, B) = appliedLens.value
  private def optic: Lens[(A, B), C] = appliedLens.optic

  def first[D](implicit ev: Field1[C, D]): AppliedLens[(A, B), D] = AppliedLens(value, optic.andThen(ev.first))

  def second[D](implicit ev: Field2[C, D]): AppliedLens[(A, B), D] = AppliedLens(value, optic.andThen(ev.second))
}

case class Tuple3ToAppliedLensOps[A, B, C](private val s: (A, B, C)) extends AnyVal {
  def first(implicit ev: Field1[(A, B, C), A]): AppliedLens[(A, B, C), A] = AppliedLens(s, ev.first)

  def second(implicit ev: Field2[(A, B, C), B]): AppliedLens[(A, B, C), B] = AppliedLens(s, ev.second)

  def third(implicit ev: Field3[(A, B, C), C]): AppliedLens[(A, B, C), C] = AppliedLens(s, ev.third)
}

case class AppliedFieldsTuple3Ops[A, B, C, D](private val appliedLens: AppliedLens[(A, B, C), D]) extends AnyVal {
  private def value: (A, B, C) = appliedLens.value
  private def optic: Lens[(A, B, C), D] = appliedLens.optic

  def first[E](implicit ev: Field1[D, E]): AppliedLens[(A, B, C), E] = AppliedLens(value, optic.andThen(ev.first))

  def second[E](implicit ev: Field2[D, E]): AppliedLens[(A, B, C), E] = AppliedLens(value, optic.andThen(ev.second))

  def third[E](implicit ev: Field3[D, E]): AppliedLens[(A, B, C), E] = AppliedLens(value, optic.andThen(ev.third))
}

case class Tuple4ToAppliedLensOps[A, B, C, D](private val s: (A, B, C, D)) extends AnyVal {
  def first(implicit ev: Field1[(A, B, C, D), A]): AppliedLens[(A, B, C, D), A] = AppliedLens(s, ev.first)

  def second(implicit ev: Field2[(A, B, C, D), B]): AppliedLens[(A, B, C, D), B] = AppliedLens(s, ev.second)

  def third(implicit ev: Field3[(A, B, C, D), C]): AppliedLens[(A, B, C, D), C] = AppliedLens(s, ev.third)

  def fourth(implicit ev: Field4[(A, B, C, D), D]): AppliedLens[(A, B, C, D), D] = AppliedLens(s, ev.fourth)
}

case class AppliedFieldsTuple4Ops[A, B, C, D, E](private val appliedLens: AppliedLens[(A, B, C, D), E]) extends AnyVal {
  private def value: (A, B, C, D) = appliedLens.value
  private def optic: Lens[(A, B, C, D), E] = appliedLens.optic

  def first[F](implicit ev: Field1[E, F]): AppliedLens[(A, B, C, D), F] = AppliedLens(value, optic.andThen(ev.first))

  def second[F](implicit ev: Field2[E, F]): AppliedLens[(A, B, C, D), F] = AppliedLens(value, optic.andThen(ev.second))

  def third[F](implicit ev: Field3[E, F]): AppliedLens[(A, B, C, D), F] = AppliedLens(value, optic.andThen(ev.third))

  def fourth[F](implicit ev: Field4[E, F]): AppliedLens[(A, B, C, D), F] = AppliedLens(value, optic.andThen(ev.fourth))
}

case class Tuple5ToAppliedLensOps[A, B, C, D, E](private val s: (A, B, C, D, E)) extends AnyVal {
  def first(implicit ev: Field1[(A, B, C, D, E), A]): AppliedLens[(A, B, C, D, E), A] = AppliedLens(s, ev.first)

  def second(implicit ev: Field2[(A, B, C, D, E), B]): AppliedLens[(A, B, C, D, E), B] = AppliedLens(s, ev.second)

  def third(implicit ev: Field3[(A, B, C, D, E), C]): AppliedLens[(A, B, C, D, E), C] = AppliedLens(s, ev.third)

  def fourth(implicit ev: Field4[(A, B, C, D, E), D]): AppliedLens[(A, B, C, D, E), D] = AppliedLens(s, ev.fourth)

  def fifth(implicit ev: Field5[(A, B, C, D, E), E]): AppliedLens[(A, B, C, D, E), E] = AppliedLens(s, ev.fifth)
}

case class AppliedFieldsTuple5Ops[A, B, C, D, E, F](private val appliedLens: AppliedLens[(A, B, C, D, E), F]) extends AnyVal {
  private def value: (A, B, C, D, E) = appliedLens.value
  private def optic: Lens[(A, B, C, D, E), F] = appliedLens.optic

  def first[G](implicit ev: Field1[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.first))

  def second[G](implicit ev: Field2[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.second))

  def third[G](implicit ev: Field3[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.third))

  def fourth[G](implicit ev: Field4[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.fourth))

  def fifth[G](implicit ev: Field5[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.fifth))
}
