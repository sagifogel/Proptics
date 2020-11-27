package proptics.syntax

trait TupleSyntax {
  implicit def tuple2TypeOps(tupleType: Tuple2.type): Tuple2TypeOps = Tuple2TypeOps(tupleType)
}

final case class Tuple2TypeOps(private val tupleType: Tuple2.type) extends AnyVal {
  /** fst convenience method for composition */
  def _1[A, B](pair: (A, B)): A = pair._1

  /** snd convenience method for composition */
  def _2[A, B](pair: (A, B)): B = pair._2

  /** swap convenience method for composition */
  def swap[A, B](pair: (A, B)): (B, A) = pair.swap
}
