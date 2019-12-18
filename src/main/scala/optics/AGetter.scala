package optics

/**
 * A [[AGetter]] is a [[Fold]] which has the same return type as the type of the target of the fold.
 *
 * @tparam S the source of an [[AGetter]]
 * @tparam T the modified source of an [[AGetter]]
 * @tparam A the target of an [[AGetter]]
 * @tparam B the modified target of an [[AGetter]]
 */
abstract class AGetter[S, T, A, B] extends Fold[A, S, T, A, B] {
}