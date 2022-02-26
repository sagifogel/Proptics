package proptics.syntax.applied

import proptics._
import proptics.applied._
import proptics.typeclass._

trait AppliedFieldsSyntax {
  implicit final def tuple2ToAppliedLensOps[A, B](s: (A, B)): Tuple2ToAppliedLensOps[A, B] = Tuple2ToAppliedLensOps(s)

  implicit final def tuple3ToAppliedLensOps[A, B, C](s: (A, B, C)): Tuple3ToAppliedLensOps[A, B, C] = Tuple3ToAppliedLensOps(s)

  implicit final def tuple4ToAppliedLensOps[A, B, C, D](s: (A, B, C, D)): Tuple4ToAppliedLensOps[A, B, C, D] = Tuple4ToAppliedLensOps(s)

  implicit final def tuple5ToAppliedLensOps[A, B, C, D, E](s: (A, B, C, D, E)): Tuple5ToAppliedLensOps[A, B, C, D, E] = Tuple5ToAppliedLensOps(s)

  implicit final def appliedLensFieldsTuple2Ops[A, B, C](appliedLens: AppliedLens[(A, B), C]): AppliedLensFieldsTuple2Ops[A, B, C] = AppliedLensFieldsTuple2Ops(appliedLens)

  implicit final def appliedLensFieldsTuple3Ops[A, B, C, D](appliedLens: AppliedLens[(A, B, C), D]): AppliedLensFieldsTuple3Ops[A, B, C, D] = AppliedLensFieldsTuple3Ops(
    appliedLens)

  implicit final def appliedLensFieldsTuple4Ops[A, B, C, D, E](appliedLens: AppliedLens[(A, B, C, D), E]): AppliedLensFieldsTuple4Ops[A, B, C, D, E] = AppliedLensFieldsTuple4Ops(
    appliedLens)

  implicit final def appliedLensTuple5Ops[A, B, C, D, E, F](appliedLens: AppliedLens[(A, B, C, D, E), F]): AppliedLensFieldsTuple5Ops[A, B, C, D, E, F] =
    AppliedLensFieldsTuple5Ops(appliedLens)

  implicit final def appliedPrismTuple2Ops[A, B, C](appliedPrism: AppliedPrism[(A, B), C]): AppliedPrismFieldsTuple2Ops[A, B, C] = AppliedPrismFieldsTuple2Ops(appliedPrism)

  implicit final def appliedPrismTuple3Ops[A, B, C, D](appliedPrism: AppliedPrism[(A, B, C), D]): AppliedPrismFieldsTuple3Ops[A, B, C, D] = AppliedPrismFieldsTuple3Ops(
    appliedPrism)

  implicit final def appliedPrismTuple4Ops[A, B, C, D, E](appliedPrism: AppliedPrism[(A, B, C, D), E]): AppliedPrismFieldsTuple4Ops[A, B, C, D, E] = AppliedPrismFieldsTuple4Ops(
    appliedPrism)

  implicit final def appliedPrismTuple5Ops[A, B, C, D, E, F](appliedPrism: AppliedPrism[(A, B, C, D, E), F]): AppliedPrismFieldsTuple5Ops[A, B, C, D, E, F] =
    AppliedPrismFieldsTuple5Ops(appliedPrism)

  implicit final def appliedAffineTraversalTuple2Ops[A, B, C](appliedAffineTraversal: AppliedAffineTraversal[(A, B), C]): AppliedAffineTraversalFieldsTuple2Ops[A, B, C] =
    AppliedAffineTraversalFieldsTuple2Ops(appliedAffineTraversal)

  implicit final def appliedAffineTraversalTuple3Ops[A, B, C, D](appliedAffineTraversal: AppliedAffineTraversal[(A, B, C), D]): AppliedAffineTraversalFieldsTuple3Ops[A, B, C, D] =
    AppliedAffineTraversalFieldsTuple3Ops(appliedAffineTraversal)

  implicit final def appliedAffineTraversalTuple4Ops[A, B, C, D, E](
      appliedAffineTraversal: AppliedAffineTraversal[(A, B, C, D), E]): AppliedAffineTraversalFieldsTuple4Ops[A, B, C, D, E] = AppliedAffineTraversalFieldsTuple4Ops(
    appliedAffineTraversal)

  implicit final def appliedAffineTraversalTuple5Ops[A, B, C, D, E, F](
      appliedAffineTraversal: AppliedAffineTraversal[(A, B, C, D, E), F]): AppliedAffineTraversalFieldsTuple5Ops[A, B, C, D, E, F] = AppliedAffineTraversalFieldsTuple5Ops(
    appliedAffineTraversal)

  implicit final def appliedFoldTuple2Ops[A, B, C](appliedFold: AppliedFold[(A, B), C]): AppliedFoldFieldsTuple2Ops[A, B, C] = AppliedFoldFieldsTuple2Ops(appliedFold)

  implicit final def appliedFoldTuple3Ops[A, B, C, D](appliedFold: AppliedFold[(A, B, C), D]): AppliedFoldFieldsTuple3Ops[A, B, C, D] = AppliedFoldFieldsTuple3Ops(appliedFold)

  implicit final def appliedFoldTuple4Ops[A, B, C, D, E](appliedFold: AppliedFold[(A, B, C, D), E]): AppliedFoldFieldsTuple4Ops[A, B, C, D, E] = AppliedFoldFieldsTuple4Ops(
    appliedFold)

  implicit final def appliedFoldTuple5Ops[A, B, C, D, E, F](appliedFold: AppliedFold[(A, B, C, D, E), F]): AppliedFoldFieldsTuple5Ops[A, B, C, D, E, F] =
    AppliedFoldFieldsTuple5Ops(appliedFold)

  implicit final def appliedTraversalTuple2Ops[A, B, C](appliedTraversal: AppliedTraversal[(A, B), C]): AppliedTraversalFieldsTuple2Ops[A, B, C] = AppliedTraversalFieldsTuple2Ops(
    appliedTraversal)

  implicit final def appliedTraversalTuple3Ops[A, B, C, D](appliedTraversal: AppliedTraversal[(A, B, C), D]): AppliedTraversalFieldsTuple3Ops[A, B, C, D] =
    AppliedTraversalFieldsTuple3Ops(appliedTraversal)

  implicit final def appliedTraversalTuple4Ops[A, B, C, D, E](appliedTraversal: AppliedTraversal[(A, B, C, D), E]): AppliedTraversalFieldsTuple4Ops[A, B, C, D, E] =
    AppliedTraversalFieldsTuple4Ops(appliedTraversal)

  implicit final def appliedTraversalTuple5Ops[A, B, C, D, E, F](appliedTraversal: AppliedTraversal[(A, B, C, D, E), F]): AppliedTraversalFieldsTuple5Ops[A, B, C, D, E, F] =
    AppliedTraversalFieldsTuple5Ops(appliedTraversal)
}

final case class Tuple2ToAppliedLensOps[A, B](private val s: (A, B)) extends AnyVal {
  /** select the first element of a Tuple2 */
  def first[D](implicit ev: Field1[(A, B), A]): AppliedLens[(A, B), A] = AppliedLens(s, ev.first)

  /** select the second element of a Tuple2 */
  def second[D](implicit ev: Field2[(A, B), B]): AppliedLens[(A, B), B] = AppliedLens(s, ev.second)
}

final case class Tuple3ToAppliedLensOps[A, B, C](private val s: (A, B, C)) extends AnyVal {
  /** select the first element of a Tuple3 */
  def first(implicit ev: Field1[(A, B, C), A]): AppliedLens[(A, B, C), A] = AppliedLens(s, ev.first)

  /** select the second element of a Tuple3 */
  def second(implicit ev: Field2[(A, B, C), B]): AppliedLens[(A, B, C), B] = AppliedLens(s, ev.second)

  /** select the third element of a Tuple3 */
  def third(implicit ev: Field3[(A, B, C), C]): AppliedLens[(A, B, C), C] = AppliedLens(s, ev.third)
}

final case class Tuple4ToAppliedLensOps[A, B, C, D](private val s: (A, B, C, D)) extends AnyVal {
  /** select the first element of a Tuple4 */
  def first(implicit ev: Field1[(A, B, C, D), A]): AppliedLens[(A, B, C, D), A] = AppliedLens(s, ev.first)

  /** select the second element of a Tuple4 */
  def second(implicit ev: Field2[(A, B, C, D), B]): AppliedLens[(A, B, C, D), B] = AppliedLens(s, ev.second)

  /** select the third element of a Tuple4 */
  def third(implicit ev: Field3[(A, B, C, D), C]): AppliedLens[(A, B, C, D), C] = AppliedLens(s, ev.third)

  /** select the fourth element of a Tuple4 */
  def fourth(implicit ev: Field4[(A, B, C, D), D]): AppliedLens[(A, B, C, D), D] = AppliedLens(s, ev.fourth)
}

final case class Tuple5ToAppliedLensOps[A, B, C, D, E](private val s: (A, B, C, D, E)) extends AnyVal {
  /** select the first element of a Tuple5 */
  def first(implicit ev: Field1[(A, B, C, D, E), A]): AppliedLens[(A, B, C, D, E), A] = AppliedLens(s, ev.first)

  /** select the second element of a Tuple5 */
  def second(implicit ev: Field2[(A, B, C, D, E), B]): AppliedLens[(A, B, C, D, E), B] = AppliedLens(s, ev.second)

  /** select the third element of a Tuple5 */
  def third(implicit ev: Field3[(A, B, C, D, E), C]): AppliedLens[(A, B, C, D, E), C] = AppliedLens(s, ev.third)

  /** select the fourth element of a Tuple5 */
  def fourth(implicit ev: Field4[(A, B, C, D, E), D]): AppliedLens[(A, B, C, D, E), D] = AppliedLens(s, ev.fourth)

  /** select the fifth element of a Tuple5 */
  def fifth(implicit ev: Field5[(A, B, C, D, E), E]): AppliedLens[(A, B, C, D, E), E] = AppliedLens(s, ev.fifth)
}

final case class AppliedLensFieldsTuple2Ops[A, B, C](private val appliedLens: AppliedLens[(A, B), C]) extends AnyVal {
  private def value: (A, B) = appliedLens.value
  private def optic: Lens[(A, B), C] = appliedLens.optic

  /** select the first element of a Tuple2 */
  def first[D](implicit ev: Field1[C, D]): AppliedLens[(A, B), D] = AppliedLens(value, optic.andThen(ev.first))

  /** select the second element of a Tuple2 */
  def second[D](implicit ev: Field2[C, D]): AppliedLens[(A, B), D] = AppliedLens(value, optic.andThen(ev.second))
}

final case class AppliedLensFieldsTuple3Ops[A, B, C, D](private val appliedLens: AppliedLens[(A, B, C), D]) extends AnyVal {
  private def value: (A, B, C) = appliedLens.value
  private def optic: Lens[(A, B, C), D] = appliedLens.optic

  /** select the first element of a Tuple3 */
  def first[E](implicit ev: Field1[D, E]): AppliedLens[(A, B, C), E] = AppliedLens(value, optic.andThen(ev.first))

  /** select the second element of a Tuple3 */
  def second[E](implicit ev: Field2[D, E]): AppliedLens[(A, B, C), E] = AppliedLens(value, optic.andThen(ev.second))

  /** select the third element of a Tuple3 */
  def third[E](implicit ev: Field3[D, E]): AppliedLens[(A, B, C), E] = AppliedLens(value, optic.andThen(ev.third))
}

final case class AppliedLensFieldsTuple4Ops[A, B, C, D, E](private val appliedLens: AppliedLens[(A, B, C, D), E]) extends AnyVal {
  private def value: (A, B, C, D) = appliedLens.value
  private def optic: Lens[(A, B, C, D), E] = appliedLens.optic

  /** select the first element of a Tuple4 */
  def first[F](implicit ev: Field1[E, F]): AppliedLens[(A, B, C, D), F] = AppliedLens(value, optic.andThen(ev.first))

  /** select the second element of a Tuple4 */
  def second[F](implicit ev: Field2[E, F]): AppliedLens[(A, B, C, D), F] = AppliedLens(value, optic.andThen(ev.second))

  /** select the third element of a Tuple4 */
  def third[F](implicit ev: Field3[E, F]): AppliedLens[(A, B, C, D), F] = AppliedLens(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple4 */
  def fourth[F](implicit ev: Field4[E, F]): AppliedLens[(A, B, C, D), F] = AppliedLens(value, optic.andThen(ev.fourth))
}

final case class AppliedLensFieldsTuple5Ops[A, B, C, D, E, F](private val appliedLens: AppliedLens[(A, B, C, D, E), F]) extends AnyVal {
  private def value: (A, B, C, D, E) = appliedLens.value
  private def optic: Lens[(A, B, C, D, E), F] = appliedLens.optic

  /** select the first element of a Tuple5 */
  def first[G](implicit ev: Field1[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.first))

  /** select the second element of a Tuple5 */
  def second[G](implicit ev: Field2[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.second))

  /** select the third element of a Tuple5 */
  def third[G](implicit ev: Field3[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple5 */
  def fourth[G](implicit ev: Field4[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.fourth))

  /** select the fifth element of a Tuple5 */
  def fifth[G](implicit ev: Field5[F, G]): AppliedLens[(A, B, C, D, E), G] = AppliedLens(value, optic.andThen(ev.fifth))
}

final case class AppliedPrismFieldsTuple2Ops[A, B, C](private val appliedPrism: AppliedPrism[(A, B), C]) extends AnyVal {
  private def value: (A, B) = appliedPrism.value
  private def optic: Prism[(A, B), C] = appliedPrism.optic

  /** select the first element of a Tuple2 */
  def first[D](implicit ev: Field1[C, D]): AppliedAffineTraversal[(A, B), D] = AppliedAffineTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple2 */
  def second[D](implicit ev: Field2[C, D]): AppliedAffineTraversal[(A, B), D] = AppliedAffineTraversal(value, optic.andThen(ev.second))
}

final case class AppliedPrismFieldsTuple3Ops[A, B, C, D](private val appliedPrism: AppliedPrism[(A, B, C), D]) extends AnyVal {
  private def value: (A, B, C) = appliedPrism.value
  private def optic: Prism[(A, B, C), D] = appliedPrism.optic

  /** select the first element of a Tuple3 */
  def first[E](implicit ev: Field1[D, E]): AppliedAffineTraversal[(A, B, C), E] = AppliedAffineTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple3 */
  def second[E](implicit ev: Field2[D, E]): AppliedAffineTraversal[(A, B, C), E] = AppliedAffineTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple3 */
  def third[E](implicit ev: Field3[D, E]): AppliedAffineTraversal[(A, B, C), E] = AppliedAffineTraversal(value, optic.andThen(ev.third))
}

final case class AppliedPrismFieldsTuple4Ops[A, B, C, D, E](private val appliedPrism: AppliedPrism[(A, B, C, D), E]) extends AnyVal {
  private def value: (A, B, C, D) = appliedPrism.value
  private def optic: Prism[(A, B, C, D), E] = appliedPrism.optic

  /** select the first element of a Tuple4 */
  def first[F](implicit ev: Field1[E, F]): AppliedAffineTraversal[(A, B, C, D), F] = AppliedAffineTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple4 */
  def second[F](implicit ev: Field2[E, F]): AppliedAffineTraversal[(A, B, C, D), F] = AppliedAffineTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple4 */
  def third[F](implicit ev: Field3[E, F]): AppliedAffineTraversal[(A, B, C, D), F] = AppliedAffineTraversal(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple4 */
  def fourth[F](implicit ev: Field4[E, F]): AppliedAffineTraversal[(A, B, C, D), F] = AppliedAffineTraversal(value, optic.andThen(ev.fourth))
}

final case class AppliedPrismFieldsTuple5Ops[A, B, C, D, E, F](private val appliedPrism: AppliedPrism[(A, B, C, D, E), F]) extends AnyVal {
  private def value: (A, B, C, D, E) = appliedPrism.value
  private def optic: Prism[(A, B, C, D, E), F] = appliedPrism.optic

  /** select the first element of a Tuple5 */
  def first[G](implicit ev: Field1[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple5 */
  def second[G](implicit ev: Field2[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple5 */
  def third[G](implicit ev: Field3[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple5 */
  def fourth[G](implicit ev: Field4[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.fourth))

  /** select the fifth element of a Tuple5 */
  def fifth[G](implicit ev: Field5[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.fifth))
}

final case class AppliedAffineTraversalFieldsTuple2Ops[A, B, C](private val appliedAffineTraversal: AppliedAffineTraversal[(A, B), C]) extends AnyVal {
  private def value: (A, B) = appliedAffineTraversal.value
  private def optic: AffineTraversal[(A, B), C] = appliedAffineTraversal.optic

  /** select the first element of a Tuple2 */
  def first[D](implicit ev: Field1[C, D]): AppliedAffineTraversal[(A, B), D] = AppliedAffineTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple2 */
  def second[D](implicit ev: Field2[C, D]): AppliedAffineTraversal[(A, B), D] = AppliedAffineTraversal(value, optic.andThen(ev.second))
}

final case class AppliedAffineTraversalFieldsTuple3Ops[A, B, C, D](private val appliedAffineTraversal: AppliedAffineTraversal[(A, B, C), D]) extends AnyVal {
  private def value: (A, B, C) = appliedAffineTraversal.value
  private def optic: AffineTraversal[(A, B, C), D] = appliedAffineTraversal.optic

  /** select the first element of a Tuple3 */
  def first[E](implicit ev: Field1[D, E]): AppliedAffineTraversal[(A, B, C), E] = AppliedAffineTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple3 */
  def second[E](implicit ev: Field2[D, E]): AppliedAffineTraversal[(A, B, C), E] = AppliedAffineTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple3 */
  def third[E](implicit ev: Field3[D, E]): AppliedAffineTraversal[(A, B, C), E] = AppliedAffineTraversal(value, optic.andThen(ev.third))
}

final case class AppliedAffineTraversalFieldsTuple4Ops[A, B, C, D, E](private val appliedAffineTraversal: AppliedAffineTraversal[(A, B, C, D), E]) extends AnyVal {
  private def value: (A, B, C, D) = appliedAffineTraversal.value
  private def optic: AffineTraversal[(A, B, C, D), E] = appliedAffineTraversal.optic

  /** select the first element of a Tuple4 */
  def first[F](implicit ev: Field1[E, F]): AppliedAffineTraversal[(A, B, C, D), F] = AppliedAffineTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple4 */
  def second[F](implicit ev: Field2[E, F]): AppliedAffineTraversal[(A, B, C, D), F] = AppliedAffineTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple4 */
  def third[F](implicit ev: Field3[E, F]): AppliedAffineTraversal[(A, B, C, D), F] = AppliedAffineTraversal(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple4 */
  def fourth[F](implicit ev: Field4[E, F]): AppliedAffineTraversal[(A, B, C, D), F] = AppliedAffineTraversal(value, optic.andThen(ev.fourth))
}

final case class AppliedAffineTraversalFieldsTuple5Ops[A, B, C, D, E, F](private val appliedAffineTraversal: AppliedAffineTraversal[(A, B, C, D, E), F]) extends AnyVal {
  private def value: (A, B, C, D, E) = appliedAffineTraversal.value
  private def optic: AffineTraversal[(A, B, C, D, E), F] = appliedAffineTraversal.optic

  /** select the first element of a Tuple5 */
  def first[G](implicit ev: Field1[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple5 */
  def second[G](implicit ev: Field2[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple5 */
  def third[G](implicit ev: Field3[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple5 */
  def fourth[G](implicit ev: Field4[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.fourth))

  /** select the fifth element of a Tuple5 */
  def fifth[G](implicit ev: Field5[F, G]): AppliedAffineTraversal[(A, B, C, D, E), G] = AppliedAffineTraversal(value, optic.andThen(ev.fifth))
}

final case class AppliedFoldFieldsTuple2Ops[A, B, C](private val appliedFold: AppliedFold[(A, B), C]) extends AnyVal {
  private def value: (A, B) = appliedFold.value
  private def optic: Fold[(A, B), C] = appliedFold.optic

  /** select the first element of a Tuple2 */
  def first[D](implicit ev: Field1[C, D]): AppliedFold[(A, B), D] = AppliedFold(value, optic.andThen(ev.first))

  /** select the second element of a Tuple2 */
  def second[D](implicit ev: Field2[C, D]): AppliedFold[(A, B), D] = AppliedFold(value, optic.andThen(ev.second))
}

final case class AppliedFoldFieldsTuple3Ops[A, B, C, D](private val appliedFold: AppliedFold[(A, B, C), D]) extends AnyVal {
  private def value: (A, B, C) = appliedFold.value
  private def optic: Fold[(A, B, C), D] = appliedFold.optic

  /** select the first element of a Tuple3 */
  def first[E](implicit ev: Field1[D, E]): AppliedFold[(A, B, C), E] = AppliedFold(value, optic.andThen(ev.first))

  /** select the second element of a Tuple3 */
  def second[E](implicit ev: Field2[D, E]): AppliedFold[(A, B, C), E] = AppliedFold(value, optic.andThen(ev.second))

  /** select the third element of a Tuple3 */
  def third[E](implicit ev: Field3[D, E]): AppliedFold[(A, B, C), E] = AppliedFold(value, optic.andThen(ev.third))
}

final case class AppliedFoldFieldsTuple4Ops[A, B, C, D, E](private val appliedFold: AppliedFold[(A, B, C, D), E]) extends AnyVal {
  private def value: (A, B, C, D) = appliedFold.value
  private def optic: Fold[(A, B, C, D), E] = appliedFold.optic

  /** select the first element of a Tuple4 */
  def first[F](implicit ev: Field1[E, F]): AppliedFold[(A, B, C, D), F] = AppliedFold(value, optic.andThen(ev.first))

  /** select the second element of a Tuple4 */
  def second[F](implicit ev: Field2[E, F]): AppliedFold[(A, B, C, D), F] = AppliedFold(value, optic.andThen(ev.second))

  /** select the third element of a Tuple4 */
  def third[F](implicit ev: Field3[E, F]): AppliedFold[(A, B, C, D), F] = AppliedFold(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple4 */
  def fourth[F](implicit ev: Field4[E, F]): AppliedFold[(A, B, C, D), F] = AppliedFold(value, optic.andThen(ev.fourth))
}

final case class AppliedFoldFieldsTuple5Ops[A, B, C, D, E, F](private val appliedFold: AppliedFold[(A, B, C, D, E), F]) extends AnyVal {
  private def value: (A, B, C, D, E) = appliedFold.value
  private def optic: Fold[(A, B, C, D, E), F] = appliedFold.optic

  /** select the first element of a Tuple5 */
  def first[G](implicit ev: Field1[F, G]): AppliedFold[(A, B, C, D, E), G] = AppliedFold(value, optic.andThen(ev.first))

  /** select the second element of a Tuple5 */
  def second[G](implicit ev: Field2[F, G]): AppliedFold[(A, B, C, D, E), G] = AppliedFold(value, optic.andThen(ev.second))

  /** select the third element of a Tuple5 */
  def third[G](implicit ev: Field3[F, G]): AppliedFold[(A, B, C, D, E), G] = AppliedFold(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple5 */
  def fourth[G](implicit ev: Field4[F, G]): AppliedFold[(A, B, C, D, E), G] = AppliedFold(value, optic.andThen(ev.fourth))

  /** select the fifth element of a Tuple5 */
  def fifth[G](implicit ev: Field5[F, G]): AppliedFold[(A, B, C, D, E), G] = AppliedFold(value, optic.andThen(ev.fifth))
}

final case class AppliedTraversalFieldsTuple2Ops[A, B, C](private val appliedTraversal: AppliedTraversal[(A, B), C]) extends AnyVal {
  private def value: (A, B) = appliedTraversal.value
  private def optic: Traversal[(A, B), C] = appliedTraversal.optic

  /** select the first element of a Tuple2 */
  def first[D](implicit ev: Field1[C, D]): AppliedTraversal[(A, B), D] = AppliedTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple2 */
  def second[D](implicit ev: Field2[C, D]): AppliedTraversal[(A, B), D] = AppliedTraversal(value, optic.andThen(ev.second))
}

final case class AppliedTraversalFieldsTuple3Ops[A, B, C, D](private val appliedTraversal: AppliedTraversal[(A, B, C), D]) extends AnyVal {
  private def value: (A, B, C) = appliedTraversal.value
  private def optic: Traversal[(A, B, C), D] = appliedTraversal.optic

  /** select the first element of a Tuple3 */
  def first[E](implicit ev: Field1[D, E]): AppliedTraversal[(A, B, C), E] = AppliedTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple3 */
  def second[E](implicit ev: Field2[D, E]): AppliedTraversal[(A, B, C), E] = AppliedTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple3 */
  def third[E](implicit ev: Field3[D, E]): AppliedTraversal[(A, B, C), E] = AppliedTraversal(value, optic.andThen(ev.third))
}

final case class AppliedTraversalFieldsTuple4Ops[A, B, C, D, E](private val appliedTraversal: AppliedTraversal[(A, B, C, D), E]) extends AnyVal {
  private def value: (A, B, C, D) = appliedTraversal.value
  private def optic: Traversal[(A, B, C, D), E] = appliedTraversal.optic

  /** select the first element of a Tuple4 */
  def first[F](implicit ev: Field1[E, F]): AppliedTraversal[(A, B, C, D), F] = AppliedTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple4 */
  def second[F](implicit ev: Field2[E, F]): AppliedTraversal[(A, B, C, D), F] = AppliedTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple4 */
  def third[F](implicit ev: Field3[E, F]): AppliedTraversal[(A, B, C, D), F] = AppliedTraversal(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple4 */
  def fourth[F](implicit ev: Field4[E, F]): AppliedTraversal[(A, B, C, D), F] = AppliedTraversal(value, optic.andThen(ev.fourth))
}

final case class AppliedTraversalFieldsTuple5Ops[A, B, C, D, E, F](private val appliedTraversal: AppliedTraversal[(A, B, C, D, E), F]) extends AnyVal {
  private def value: (A, B, C, D, E) = appliedTraversal.value
  private def optic: Traversal[(A, B, C, D, E), F] = appliedTraversal.optic

  /** select the first element of a Tuple5 */
  def first[G](implicit ev: Field1[F, G]): AppliedTraversal[(A, B, C, D, E), G] = AppliedTraversal(value, optic.andThen(ev.first))

  /** select the second element of a Tuple5 */
  def second[G](implicit ev: Field2[F, G]): AppliedTraversal[(A, B, C, D, E), G] = AppliedTraversal(value, optic.andThen(ev.second))

  /** select the third element of a Tuple5 */
  def third[G](implicit ev: Field3[F, G]): AppliedTraversal[(A, B, C, D, E), G] = AppliedTraversal(value, optic.andThen(ev.third))

  /** select the fourth element of a Tuple5 */
  def fourth[G](implicit ev: Field4[F, G]): AppliedTraversal[(A, B, C, D, E), G] = AppliedTraversal(value, optic.andThen(ev.fourth))

  /** select the fifth element of a Tuple5 */
  def fifth[G](implicit ev: Field5[F, G]): AppliedTraversal[(A, B, C, D, E), G] = AppliedTraversal(value, optic.andThen(ev.fifth))
}
