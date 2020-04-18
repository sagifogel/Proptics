package proptics.syntax

object Tuple2Syntax {
  private[proptics] implicit final class Tuple2Ops(private val value: Tuple2.type) extends AnyVal {
    def _2[A, B](pair: (A, B)): B = pair._2
  }
}
