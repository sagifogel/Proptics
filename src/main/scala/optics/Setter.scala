package optics

/**
 * A [[Setter]] is an [[Optic]] with a fixed type of a [[Function1]] as the type constructor
 *
 * @tparam S the source of a [[Setter]]
 * @tparam T the modified source of a [[Setter]]
 * @tparam A the target of a [[Setter]]
 * @tparam B the modified target of a [[Setter]]
 */
abstract class Setter[S, T, A, B] extends Optic[* => *, S, T, A, B] {
}
