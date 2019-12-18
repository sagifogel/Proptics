package optics

/**
 * A [[Getter]] is a [[Fold]]
 *
 * @tparam R the return type of a [[Getter]]
 * @tparam S the source of a [[Getter]]
 * @tparam T the modified source of a [[Getter]]
 * @tparam A the target of a [[Getter]]
 * @tparam B the modified target of a [[Getter]]
 */
abstract class Getter[R, S, T, A, B] extends Fold[R, S, T, A, B] {
}
