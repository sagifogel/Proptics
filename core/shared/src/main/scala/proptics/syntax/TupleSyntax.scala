package proptics.syntax

trait TupleSyntax {
  implicit def tuple2TypeOps(tupleType: Tuple2.type) = Tuple2TypeOps(tupleType)
}

final case class Tuple2TypeOps(private val tupleType: Tuple2.type) extends AnyVal {
  def _1[A, B](pair: (A, B)): A = pair._1

  def _2[A, B](pair: (A, B)): B = pair._2
}
