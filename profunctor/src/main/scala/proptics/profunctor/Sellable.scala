package proptics.profunctor

trait Sellable[P[_, _], W[_, _, _]] extends Serializable {
  def sell[A, B]: P[A, W[A, B, B]]
}
