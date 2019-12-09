package optics

/**
 * A general-purpose Lens
 *
 * @tparam P a type constructor of (* -> * -> *)
 * @tparam S the source of an [[Optic]]
 * @tparam T the modified source of a [[Optic]]
 * @tparam A the target of a [[Optic]]
 * @tparam B the modified target of a [[Optic]]
 */
private[optics] abstract class Optic[P[_, _], S, T, A, B] extends Serializable {
  val pab: P[A, B] => P[S, T]
}

object Optic {
  def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T]): Optic[P, S, T, A, B] = new Optic[P, S, T, A, B] {
    val pab: P[A, B] => P[S, T] = f
  }
}

object Optic_ {
  def apply[P[_, _], S, A](pab: P[A, A] => P[S, S]): Optic_[P, S, A] = Optic(pab)
}