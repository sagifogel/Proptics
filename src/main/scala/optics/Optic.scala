package optics

/**
 * A general-purpose Lens
 *
 * @tparam P a type constructor of kind (* -> * -> *)
 * @tparam S the source of an [[Optic]]
 * @tparam T the modified source of an [[Optic]]
 * @tparam A the target of an [[Optic]]
 * @tparam B the modified target of an [[Optic]]
 */
private[optics] abstract class Optic[P[_, _], S, T, A, B] extends Serializable {
  def apply(pab: P[A, B]): P[S, T]
}

object Optic {
  def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T]): Optic[P, S, T, A, B] = (pab: P[A, B]) => f(pab)
}

object Optic_ {
  def apply[P[_, _], S, A](pab: P[A, A] => P[S, S]): Optic_[P, S, A] = Optic(pab)
}