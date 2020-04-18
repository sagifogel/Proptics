package proptics.syntax

object Tuple2Syntax {
  private[proptics] implicit final class Tuple2Ops[A, B](private val value: Tuple2.type) extends AnyVal {
    def _2(pair: (A, B)): B = pair._2
  }
}
